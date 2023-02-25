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

// Base for Statement Nodes
type Statement interface {
	Node
	statementNode()
}

// Base for Expression Nodes
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

	for _, str := range prog.Statements {
		out.WriteString(str.String())
	}

	return out.String()
}

type LetStatement struct {
	Token token.Token
	Name  *Identifier
	Value Expression
}

func (letstmt *LetStatement) statementNode()       {}
func (letstmt *LetStatement) TokenLiteral() string { return letstmt.Token.Literal }
func (letstmt *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(letstmt.TokenLiteral() + " ")
	out.WriteString(letstmt.Name.String())
	out.WriteString(" = ")

	if letstmt.Value != nil {
		out.WriteString(letstmt.Value.String())
	}

	out.WriteString(";")

	return out.String()
}

type ReturnStatement struct {
	Token       token.Token
	ReturnValue Expression
}

func (retstmt *ReturnStatement) statementNode()       {}
func (retstmt *ReturnStatement) TokenLiteral() string { return retstmt.Token.Literal }
func (retstmt *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(retstmt.TokenLiteral() + " ")

	if retstmt.ReturnValue != nil {
		out.WriteString(retstmt.ReturnValue.String())
	}

	out.WriteString(";")

	return out.String()
}

type ExpressionStatement struct {
	Token      token.Token
	Expression Expression
}

func (expstmt *ExpressionStatement) statementNode()       {}
func (expstmt *ExpressionStatement) TokenLiteral() string { return expstmt.Token.Literal }
func (expstmt *ExpressionStatement) String() string {

	if expstmt.Expression != nil {
		return expstmt.Expression.String()
	}

	return ""
}

type BlockStatement struct {
	Token      token.Token
	Statements []Statement
}

func (blkstmt *BlockStatement) statementNode()       {}
func (blkstmt *BlockStatement) TokenLiteral() string { return blkstmt.Token.Literal }
func (blkstmt *BlockStatement) String() string {
	var out bytes.Buffer

	for _, str := range blkstmt.Statements {
		out.WriteString(str.String())
	}

	return out.String()
}

// Expressions
type Identifier struct {
	Token token.Token
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

func (intlit *IntegerLiteral) expressionNode()      {}
func (intlit *IntegerLiteral) TokenLiteral() string { return intlit.Token.Literal }
func (intlit *IntegerLiteral) String() string       { return intlit.Token.Literal }

type PrefixExpression struct {
	Token    token.Token // Prefix token, e.g. !
	Operator string
	Right    Expression
}

func (pexp *PrefixExpression) expressionNode()      {}
func (pexp *PrefixExpression) TokenLiteral() string { return pexp.Token.Literal }
func (pexp *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pexp.Operator)
	out.WriteString(pexp.Right.String())

	return out.String()
}

type InfixExpression struct {
	Token    token.Token // Infix operator, e.g. +
	Left     Expression
	Operator string
	Right    Expression
}

func (inexpr *InfixExpression) expressionNode()      {}
func (inexpr *InfixExpression) TokenLiteral() string { return inexpr.Token.Literal }
func (inexpr *InfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(inexpr.Left.String())
	out.WriteString("" + inexpr.Operator + " ")
	out.WriteString(inexpr.Right.String())

	return out.String()
}

type IfExpression struct {
	Token       token.Token
	Condition   Expression
	Consequence *BlockStatement
	Alternative *BlockStatement
}

func (ifexpr *IfExpression) expressionNode()      {}
func (ifexpr *IfExpression) TokenLiteral() string { return ifexpr.Token.Literal }
func (ifexpr *IfExpression) String() string {
	var out bytes.Buffer

	out.WriteString("if")
	out.WriteString(ifexpr.Condition.String())
	out.WriteString(" ")
	out.WriteString(ifexpr.Consequence.String())

	if ifexpr.Alternative != nil {
		out.WriteString("else")
		out.WriteString(ifexpr.Alternative.String())
	}

	return out.String()
}

type FunctionLiteral struct {
	Token      token.Token
	Parameters []*Identifier
	Body       *BlockStatement
}

func (funcstmt *FunctionLiteral) expressionNode()      {}
func (funcstmt *FunctionLiteral) TokenLiteral() string { return funcstmt.Token.Literal }
func (funcstmt *FunctionLiteral) String() string {
	var out bytes.Buffer

	params := []string{}
	for _, p := range funcstmt.Parameters {
		params = append(params, p.String())
	}

	out.WriteString(funcstmt.TokenLiteral())
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") ")
	out.WriteString(funcstmt.Body.String())

	return out.String()
}

type CallExpression struct {
	Token     token.Token
	Function  Expression
	Arguments []Expression
}

func (callexp *CallExpression) expressionNode()      {}
func (callexp *CallExpression) TokenLiteral() string { return callexp.Token.Literal }
func (callexp *CallExpression) String() string {
	var out bytes.Buffer

	args := []string{}
	for _, a := range callexp.Arguments {
		args = append(args, a.String())
	}

	out.WriteString(callexp.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")

	return out.String()
}
