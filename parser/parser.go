package parser

import (
	"fmt"
	"strconv"

	"github.com/JohhnMag0/Rafiki/ast"
	"github.com/JohhnMag0/Rafiki/lexer"
	"github.com/JohhnMag0/Rafiki/token"
)

const (
	_ int = iota
	LOWEST
	EQUALS      // ==
	LESSGREATER // <=, <, >=, >
	SUM         // +
	PRODUCT     // *
	PREFIX      // -X or !X
	CALL        // callFunc(x)
)

var precedences = map[token.TokenType]int{
	token.EQ:     EQUALS,
	token.NOT_EQ: EQUALS,
	token.LT:     LESSGREATER,
	token.GT:     LESSGREATER,
	token.GTEQ:   LESSGREATER,
	token.LTEQ:   LESSGREATER,
	token.PLUS:   SUM,
	token.MINUS:  SUM,
	token.MULT:   PRODUCT,
	token.DIV:    PRODUCT,
	token.LPAREN: CALL,
}

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

type Parser struct {
	lex    *lexer.Lexer
	errors []string

	curToken  token.Token
	peekToken token.Token

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

func New(lex *lexer.Lexer) *Parser {
	prs := &Parser{
		lex:    lex,
		errors: []string{},
	}

	prs.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	prs.registerPrefix(token.IDENT, prs.parseIdentifier)
	prs.registerPrefix(token.INT, prs.parseIntegerLiteral)
	prs.registerPrefix(token.NOT, prs.parsePrefixExpression)
	prs.registerPrefix(token.MINUS, prs.parsePrefixExpression)
	prs.registerPrefix(token.TRUE, prs.parseBoolean)
	prs.registerPrefix(token.FALSE, prs.parseBoolean)
	prs.registerPrefix(token.LPAREN, prs.parseGroupedExpression)
	prs.registerPrefix(token.IF, prs.parseIfExpression)
	prs.registerPrefix(token.FUNCTION, prs.parseFunctionLiteral)

	prs.infixParseFns = make(map[token.TokenType]infixParseFn)
	prs.registerInfix(token.PLUS, prs.parseInfixExpression)
	prs.registerInfix(token.MINUS, prs.parseInfixExpression)
	prs.registerInfix(token.DIV, prs.parseInfixExpression)
	prs.registerInfix(token.MULT, prs.parseInfixExpression)
	prs.registerInfix(token.EQ, prs.parseInfixExpression)
	prs.registerInfix(token.NOT_EQ, prs.parseInfixExpression)
	prs.registerInfix(token.LT, prs.parseInfixExpression)
	prs.registerInfix(token.GT, prs.parseInfixExpression)
	prs.registerInfix(token.GTEQ, prs.parseInfixExpression)
	prs.registerInfix(token.LTEQ, prs.parseInfixExpression)

	prs.registerInfix(token.LPAREN, prs.parseCallExpression)

	// Read two tokens, so we set the curToken and peekToken
	prs.nextToken()
	prs.nextToken()

	return prs
}

func (prs *Parser) nextToken() {
	prs.curToken = prs.peekToken
	prs.peekToken = prs.lex.NextToken()
}

func (prs *Parser) curTokenIs(tok token.TokenType) bool {
	return prs.curToken.Type == tok
}

func (prs *Parser) peekTokenIs(tok token.TokenType) bool {
	return prs.peekToken.Type == tok
}

func (prs *Parser) expectPeek(tok token.TokenType) bool {
	if prs.peekTokenIs(tok) {
		prs.nextToken()
		return true
	} else {
		prs.peekError(tok)
		return false
	}
}

func (prs *Parser) Errors() []string {
	return prs.errors
}

func (prs *Parser) peekError(tok token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead",
		tok, prs.peekToken.Type)
	prs.errors = append(prs.errors, msg)
}

func (prs *Parser) noPrefixParseFnError(tok token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", tok)
	prs.errors = append(prs.errors, msg)
}

func (prs *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !prs.curTokenIs(token.EOF) {
		stmt := prs.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		prs.nextToken()
	}
	return program
}

func (prs *Parser) parseStatement() ast.Statement {
	switch prs.curToken.Type {
	case token.LET:
		return prs.parseLetStatement()
	case token.RETURN:
		return prs.parseReturnStatement()
	default:
		return prs.parseExpressionStatement()
	}
}

func (prs *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: prs.curToken}

	if !prs.expectPeek(token.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: prs.curToken, Value: prs.curToken.Literal}

	if !prs.expectPeek(token.ASSIGN) {
		return nil
	}

	prs.nextToken()

	stmt.Value = prs.parseExpression(LOWEST)

	if prs.peekTokenIs(token.SEMICOLON) {
		prs.nextToken()
	}

	return stmt
}

func (prs *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: prs.curToken}

	prs.nextToken()

	stmt.ReturnValue = prs.parseExpression(LOWEST)

	if prs.peekTokenIs(token.SEMICOLON) {
		prs.nextToken()
	}

	return stmt
}

func (prs *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: prs.curToken}

	stmt.Expression = prs.parseExpression(LOWEST)

	if prs.peekTokenIs(token.SEMICOLON) {
		prs.nextToken()
	}

	return stmt
}

func (prs *Parser) parseExpression(precedence int) ast.Expression {
	prefix := prs.prefixParseFns[prs.curToken.Type]
	if prefix == nil {
		prs.noPrefixParseFnError(prs.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for !prs.peekTokenIs(token.SEMICOLON) && precedence < prs.peekPrecedence() {
		infix := prs.infixParseFns[prs.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		prs.nextToken()

		leftExp = infix(leftExp)
	}

	return leftExp
}

func (prs *Parser) peekPrecedence() int {
	if prs, ok := precedences[prs.peekToken.Type]; ok {
		return prs
	}

	return LOWEST
}

func (prs *Parser) curPrecedence() int {
	if prs, ok := precedences[prs.curToken.Type]; ok {
		return prs
	}

	return LOWEST
}

func (prs *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: prs.curToken, Value: prs.curToken.Literal}
}

func (prs *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: prs.curToken}

	value, err := strconv.ParseInt(prs.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", prs.curToken.Literal)
		prs.errors = append(prs.errors, msg)
		return nil
	}

	lit.Value = value

	return lit
}

func (prs *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    prs.curToken,
		Operator: prs.curToken.Literal,
	}

	prs.nextToken()

	expression.Right = prs.parseExpression(PREFIX)

	return expression
}

func (prs *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    prs.curToken,
		Operator: prs.curToken.Literal,
		Left:     left,
	}

	precedence := prs.curPrecedence()
	prs.nextToken()
	expression.Right = prs.parseExpression(precedence)

	return expression
}

func (prs *Parser) parseBoolean() ast.Expression {
	return &ast.Boolean{Token: prs.curToken, Value: prs.curTokenIs(token.TRUE)}
}

func (prs *Parser) parseGroupedExpression() ast.Expression {
	prs.nextToken()

	exp := prs.parseExpression(LOWEST)

	if !prs.expectPeek(token.RPAREN) {
		return nil
	}

	return exp
}

func (prs *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: prs.curToken}

	if !prs.expectPeek(token.LPAREN) {
		return nil
	}

	prs.nextToken()
	expression.Condition = prs.parseExpression(LOWEST)

	if !prs.expectPeek(token.RPAREN) {
		return nil
	}

	if !prs.expectPeek(token.LBRACK) {
		return nil
	}

	expression.Consequence = prs.parseBlockStatement()

	if prs.peekTokenIs(token.ELSE) {
		prs.nextToken()

		if !prs.expectPeek(token.LBRACK) {
			return nil
		}

		expression.Alternative = prs.parseBlockStatement()
	}

	return expression
}

func (prs *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: prs.curToken}
	block.Statements = []ast.Statement{}

	prs.nextToken()

	for !prs.curTokenIs(token.RBRACK) && !prs.curTokenIs(token.EOF) {
		stmt := prs.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		prs.nextToken()
	}

	return block
}

func (prs *Parser) parseFunctionLiteral() ast.Expression {
	lit := &ast.FunctionLiteral{Token: prs.curToken}

	if !prs.expectPeek(token.LPAREN) {
		return nil
	}

	lit.Parameters = prs.parseFunctionParameters()

	if !prs.expectPeek(token.LBRACK) {
		return nil
	}

	lit.Body = prs.parseBlockStatement()

	return lit
}

func (prs *Parser) parseFunctionParameters() []*ast.Identifier {
	identifiers := []*ast.Identifier{}

	if prs.peekTokenIs(token.RPAREN) {
		prs.nextToken()
		return identifiers
	}

	prs.nextToken()

	ident := &ast.Identifier{Token: prs.curToken, Value: prs.curToken.Literal}
	identifiers = append(identifiers, ident)

	for prs.peekTokenIs(token.COMMA) {
		prs.nextToken()
		prs.nextToken()
		ident := &ast.Identifier{Token: prs.curToken, Value: prs.curToken.Literal}
		identifiers = append(identifiers, ident)
	}

	if !prs.expectPeek(token.RPAREN) {
		return nil
	}

	return identifiers
}

func (prs *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: prs.curToken, Function: function}
	exp.Arguments = prs.parseCallArguments()
	return exp
}

func (prs *Parser) parseCallArguments() []ast.Expression {
	args := []ast.Expression{}

	if prs.peekTokenIs(token.COMMA) {
		prs.nextToken()
		prs.nextToken()
		args = append(args, prs.parseExpression(LOWEST))
	}

	if !prs.expectPeek(token.RPAREN) {
		return nil
	}

	return args
}

func (prs *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	prs.prefixParseFns[tokenType] = fn
}

func (prs *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	prs.infixParseFns[tokenType] = fn
}
