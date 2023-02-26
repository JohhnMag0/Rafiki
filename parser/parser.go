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
	LESSGREATER // <, >, <=, >=
	SUM         // +, -
	PRODUCT     // *, /
	PREFIX      // -X, !X
	CALL        // callFunc(x)
)

var precedences = map[token.TokenType]int{
	token.EQ:     EQUALS,
	token.NOT_EQ: EQUALS,
	token.LT:     LESSGREATER,
	token.GT:     LESSGREATER,
	token.LTEQ:   LESSGREATER,
	token.GTEQ:   LESSGREATER,
	token.MINUS:  SUM,
	token.PLUS:   SUM,
	token.DIV:    PRODUCT,
	token.MULT:   PRODUCT,
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
	par := &Parser{
		lex:    lex,
		errors: []string{},
	}

	par.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	par.registerPrefix(token.IDENT, par.parseIdentifier)
	par.registerPrefix(token.INT, par.parseIntegerLiteral)
	par.registerPrefix(token.NOT, par.parsePrefixExpression)
	par.registerPrefix(token.MINUS, par.parsePrefixExpression)
	par.registerPrefix(token.TRUE, par.parseBoolean)
	par.registerPrefix(token.FALSE, par.parseBoolean)
	par.registerPrefix(token.LPAREN, par.parseGroupedExpression)
	par.registerPrefix(token.IF, par.parseIfExpression)
	par.registerPrefix(token.FUNCTION, par.parseFunctionLiteral)

	par.infixParseFns = make(map[token.TokenType]infixParseFn)
	par.registerInfix(token.PLUS, par.parseInfixExpression)
	par.registerInfix(token.MINUS, par.parseInfixExpression)
	par.registerInfix(token.DIV, par.parseInfixExpression)
	par.registerInfix(token.MULT, par.parseInfixExpression)
	par.registerInfix(token.EQ, par.parseInfixExpression)
	par.registerInfix(token.NOT_EQ, par.parseInfixExpression)
	par.registerInfix(token.LT, par.parseInfixExpression)
	par.registerInfix(token.GT, par.parseInfixExpression)
	par.registerInfix(token.LTEQ, par.parseInfixExpression)
	par.registerInfix(token.GTEQ, par.parseInfixExpression)

	par.registerInfix(token.LPAREN, par.parseCallExpression)

	// Read two tokens, one for CurToken other for the peekToken
	par.nextToken()
	par.nextToken()

	return par
}

func (par *Parser) nextToken() {
	par.curToken = par.peekToken
	par.peekToken = par.lex.NextToken()
}

func (par *Parser) curTokenIs(tok token.TokenType) bool {
	return par.curToken.Type == tok
}

func (par *Parser) peekTokenIs(tok token.TokenType) bool {
	return par.peekToken.Type == tok
}

func (par *Parser) expectPeek(tok token.TokenType) bool {
	if par.peekTokenIs(tok) {
		par.nextToken()
		return true
	} else {
		par.peekError(tok)
		return false
	}
}

func (par *Parser) Errors() []string {
	return par.errors
}

func (par *Parser) peekError(tok token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead",
		tok, par.peekToken.Type)
	par.errors = append(par.errors, msg)
}

func (par *Parser) noPrefixParseFnError(tok token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", tok)
	par.errors = append(par.errors, msg)
}

func (par *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !par.curTokenIs(token.EOF) {
		stmt := par.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		par.nextToken()
	}

	return program
}

func (par *Parser) parseStatement() ast.Statement {
	switch par.curToken.Type {
	case token.LET:
		return par.parseLetStatement()
	case token.RETURN:
		return par.parseReturnStatement()
	default:
		return par.parseExpressionStatement()
	}
}

func (par *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: par.curToken}

	if !par.expectPeek(token.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: par.curToken, Value: par.curToken.Literal}

	if !par.expectPeek(token.ASSIGN) {
		return nil
	}

	par.nextToken()

	stmt.Value = par.parseExpression(LOWEST)

	if par.peekTokenIs(token.SEMICOLON) {
		par.nextToken()
	}

	return stmt
}

func (par *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: par.curToken}

	par.nextToken()

	stmt.ReturnValue = par.parseExpression(LOWEST)

	if par.peekTokenIs(token.SEMICOLON) {
		par.nextToken()
	}

	return stmt
}

func (par *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: par.curToken}

	stmt.Expression = par.parseExpression(LOWEST)

	if par.peekTokenIs(token.SEMICOLON) {
		par.nextToken()
	}

	return stmt
}

func (par *Parser) parseExpression(precedence int) ast.Expression {
	prefix := par.prefixParseFns[par.curToken.Type]
	if prefix == nil {
		par.noPrefixParseFnError(par.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for !par.peekTokenIs(token.SEMICOLON) && precedence < par.peekPrecedence() {
		infix := par.infixParseFns[par.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		par.nextToken()

		leftExp = infix(leftExp)
	}

	return leftExp
}

func (par *Parser) peekPrecedence() int {
	if par, ok := precedences[par.peekToken.Type]; ok {
		return par
	}

	return LOWEST
}

func (par *Parser) curPrecedence() int {
	if par, ok := precedences[par.curToken.Type]; ok {
		return par
	}

	return LOWEST
}

func (par *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: par.curToken, Value: par.curToken.Literal}
}

func (par *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: par.curToken}

	value, err := strconv.ParseInt(par.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", par.curToken.Literal)
		par.errors = append(par.errors, msg)
		return nil
	}

	lit.Value = value

	return lit
}

func (par *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    par.curToken,
		Operator: par.curToken.Literal,
	}

	par.nextToken()

	expression.Right = par.parseExpression(PREFIX)

	return expression
}

func (par *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    par.curToken,
		Operator: par.curToken.Literal,
		Left:     left,
	}

	precedence := par.curPrecedence()
	par.nextToken()
	expression.Right = par.parseExpression(precedence)

	return expression
}

func (par *Parser) parseBoolean() ast.Expression {
	return &ast.Boolean{Token: par.curToken, Value: par.curTokenIs(token.TRUE)}
}

func (par *Parser) parseGroupedExpression() ast.Expression {
	par.nextToken()

	exp := par.parseExpression(LOWEST)

	if !par.expectPeek(token.RPAREN) {
		return nil
	}

	return exp
}

func (par *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: par.curToken}

	if !par.expectPeek(token.LPAREN) {
		return nil
	}

	par.nextToken()
	expression.Condition = par.parseExpression(LOWEST)

	if !par.expectPeek(token.RPAREN) {
		return nil
	}

	if !par.expectPeek(token.LBRACK) {
		return nil
	}

	expression.Consequence = par.parseBlockStatement()

	if par.peekTokenIs(token.ELSE) {
		par.nextToken()

		if !par.expectPeek(token.LBRACK) {
			return nil
		}

		expression.Alternative = par.parseBlockStatement()
	}

	return expression
}

func (par *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: par.curToken}
	block.Statements = []ast.Statement{}

	par.nextToken()

	for !par.curTokenIs(token.RBRACK) && !par.curTokenIs(token.EOF) {
		stmt := par.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		par.nextToken()
	}

	return block
}

func (par *Parser) parseFunctionLiteral() ast.Expression {
	lit := &ast.FunctionLiteral{Token: par.curToken}

	if !par.expectPeek(token.LPAREN) {
		return nil
	}

	lit.Parameters = par.parseFunctionParameters()

	if !par.expectPeek(token.LBRACK) {
		return nil
	}

	lit.Body = par.parseBlockStatement()

	return lit
}

func (par *Parser) parseFunctionParameters() []*ast.Identifier {
	identifiers := []*ast.Identifier{}

	if par.peekTokenIs(token.RPAREN) {
		par.nextToken()
		return identifiers
	}

	par.nextToken()

	ident := &ast.Identifier{Token: par.curToken, Value: par.curToken.Literal}
	identifiers = append(identifiers, ident)

	for par.peekTokenIs(token.COMMA) {
		par.nextToken()
		par.nextToken()
		ident := &ast.Identifier{Token: par.curToken, Value: par.curToken.Literal}
		identifiers = append(identifiers, ident)
	}

	if !par.expectPeek(token.RPAREN) {
		return nil
	}

	return identifiers
}

func (par *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: par.curToken, Function: function}
	exp.Arguments = par.parseCallArguments()
	return exp
}

func (par *Parser) parseCallArguments() []ast.Expression {
	args := []ast.Expression{}

	if par.peekTokenIs(token.RPAREN) {
		par.nextToken()
		return args
	}

	par.nextToken()
	args = append(args, par.parseExpression(LOWEST))

	for par.peekTokenIs(token.COMMA) {
		par.nextToken()
		par.nextToken()
		args = append(args, par.parseExpression(LOWEST))
	}

	if !par.expectPeek(token.RPAREN) {
		return nil
	}

	return args
}

func (par *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	par.prefixParseFns[tokenType] = fn
}

func (par *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	par.infixParseFns[tokenType] = fn
}
