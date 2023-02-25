package ast

import (
	"bytes"
	"strings"

	"github.com/JohhnMag0/Rafiki/token"
)

// Base Node Interface
type Node interface {
	TokenLiteral() string
	String() string
}

// Base for Statements
type Statement interface {
	Node
	statementNode()
}

// Base for Expression
type Expression interface {
	Node
	expressionNode()
}

type Program struct {
	Statements []Statement
}

func (prog *Program) TokenLiteral() string {
	if len(prog.Statements) > 0 {
		return prog.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

func (prog *Program) String() string {
	var out bytes.Buffer

	for _, stmt := range prog.Statements {
		out.WriteString(stmt.String())
	}

	return out.String()
}

// Statements
type LetStatement struct {
	Token token.Token // LET
	Name  *Identifier
	Value Expression
}

func (lt *LetStatement) statementNode()       {}
func (lt *LetStatement) TokenLiteral() string { return lt.Token.Literal }
func (lt *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(lt.TokenLiteral() + " ")
	out.WriteString(lt.Name.String())
	out.WriteString(" = ")

	if lt.Value != nil {
		out.WriteString(lt.Value.String())
	}

	out.WriteString(";")

	return out.String()
}

type ReturnStatement struct {
	Token       token.Token // RETURN
	ReturnValue Expression
}

func (ret *ReturnStatement) statementNode()       {}
func (ret *ReturnStatement) TokenLiteral() string { return ret.Token.Literal }
func (ret *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ret.TokenLiteral() + " ")

	if ret.ReturnValue != nil {
		out.WriteString(ret.ReturnValue.String())
	}

	out.WriteString(";")

	return out.String()
}

type ExpressionStatement struct {
	Token      token.Token
	Expression Expression
}

func (exp *ExpressionStatement) statementNode()       {}
func (exp *ExpressionStatement) TokenLiteral() string { return exp.Token.Literal }
func (exp *ExpressionStatement) String() string {
	if exp.Expression != nil {
		return exp.Expression.String()
	}

	return ""
}

type BlockStatement struct {
	Token     token.Token // {
	Statement []Statement
}

func (blk *BlockStatement) statementNode()       {}
func (blk *BlockStatement) TokenLiteral() string { return blk.Token.Literal }
func (blk *BlockStatement) String() string {
	var out bytes.Buffer

	for _, stmt := range blk.Statement {
		out.WriteString(stmt.String())
	}

	return out.String()
}

// Expressions
type Identifier struct {
	Token token.Token // IDENT
	Value string
}

func (id *Identifier) expressionNode()      {}
func (id *Identifier) TokenLiteral() string { return id.Token.Literal }
func (id *Identifier) String() string       { return id.Value }

type Boolean struct {
	Token token.Token
	Value bool
}

func (bolen *Boolean) expressionNode()      {}
func (bolen *Boolean) TokenLiteral() string { return bolen.Token.Literal }
func (bolen *Boolean) String() string       { return bolen.Token.Literal }

type IntegerLiteral struct {
	Token token.Token
	Value int64
}

func (inl *IntegerLiteral) expressionNode()      {}
func (inl *IntegerLiteral) TokenLiteral() string { return inl.Token.Literal }
func (inl *IntegerLiteral) String() string       { return inl.Token.Literal }

type PrefixExpression struct {
	Token    token.Token // e.g. '!'
	Operator string
	Right    Expression
}

func (prexp *PrefixExpression) expressionNode()      {}
func (prexp *PrefixExpression) TokenLiteral() string { return prexp.Token.Literal }
func (prexp *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(prexp.Operator)
	out.WriteString(prexp.Right.String())
	out.WriteString(")")

	return out.String()
}

type InfixExpression struct {
	Token    token.Token // e.g. '+'
	Left     Expression
	Operator string
	Right    Expression
}

func (inexp *InfixExpression) expressionNode()      {}
func (inexp *InfixExpression) TokenLiteral() string { return inexp.Token.Literal }
func (inexp *InfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(inexp.Left.String())
	out.WriteString(" " + inexp.Operator + " ")
	out.WriteString(inexp.Right.String())
	out.WriteString(")")

	return out.String()
}

type IfExpression struct {
	Token       token.Token // IF
	Condition   Expression
	Consequence *BlockStatement
	Alternative *BlockStatement
}

func (ifexp *IfExpression) expressionNode()      {}
func (ifexp *IfExpression) TokenLiteral() string { return ifexp.Token.Literal }
func (ifexp *IfExpression) String() string {
	var out bytes.Buffer

	out.WriteString("if")
	out.WriteString(ifexp.Condition.String())
	out.WriteString(" ")
	out.WriteString(ifexp.Consequence.String())

	if ifexp.Alternative != nil {
		out.WriteString("else")
		out.WriteString(ifexp.Alternative.String())
	}

	return out.String()
}

type FunctionLiteral struct {
	Token      token.Token // FUNC
	Parameters []*Identifier
	Body       *BlockStatement
}

func (funli *FunctionLiteral) expressionNode()      {}
func (funli *FunctionLiteral) TokenLiteral() string { return funli.Token.Literal }
func (funli *FunctionLiteral) String() string {
	var out bytes.Buffer

	params := []string{}
	for _, par := range funli.Parameters {
		params = append(params, par.String())
	}

	out.WriteString(funli.TokenLiteral())
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") ")
	out.WriteString(funli.Body.String())

	return out.String()
}

type CallExpression struct {
	Token     token.Token // '('
	Function  Expression
	Arguments []Expression
}

func (calexp *CallExpression) expressionNode()      {}
func (calexp *CallExpression) TokenLiteral() string { return calexp.Token.Literal }
func (calexp *CallExpression) String() string {
	var out bytes.Buffer

	args := []string{}
	for _, ar := range calexp.Arguments {
		args = append(args, ar.String())
	}

	out.WriteString(calexp.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")

	return out.String()
}
