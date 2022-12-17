use serde::{Serialize, Serializer};

use super::{
    bitwise_expr::BitwiseExpr,
    index_expr::IndexExpr,
    visitor::{Visitor, VisitorMut},
    ValueExpr,
};
use crate::{
    compiler::Compiler,
    execution_context::ExecutionContext,
    filter::{CompiledExpr, CompiledValueExpr},
    lex::{LexResult, LexWith},
    scheme::Scheme,
    types::{GetType, LhsValue, Type},
};

/// ValueExprWrapper is an expr that should be finally destructured into a
/// LhsFieldExpr.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ValueExprWrapper<'s> {
    /// Basic IndexExpr
    IndexExpr(IndexExpr<'s>),
    /// BitwiseExpr consisting of an IndexExpr and a bitwise operation over an
    /// rhs literal
    BitwiseExpr(BitwiseExpr<'s>),
}

// don't show the node in the serialized output
impl<'s> Serialize for ValueExprWrapper<'s> {
    fn serialize<S: Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        match self {
            ValueExprWrapper::BitwiseExpr(expr) => expr.serialize(ser),
            ValueExprWrapper::IndexExpr(expr) => expr.serialize(ser),
        }
    }
}

impl<'s> TryFrom<ValueExprWrapper<'s>> for IndexExpr<'s> {
    type Error = &'static str;

    fn try_from(value: ValueExprWrapper<'s>) -> Result<Self, Self::Error> {
        match value {
            ValueExprWrapper::IndexExpr(v) => Ok(v),
            _ => Err("Wrong kind of ValueExpr supplied"),
        }
    }
}

impl<'s> TryFrom<ValueExprWrapper<'s>> for BitwiseExpr<'s> {
    type Error = &'static str;

    fn try_from(value: ValueExprWrapper<'s>) -> Result<Self, Self::Error> {
        match value {
            ValueExprWrapper::BitwiseExpr(v) => Ok(v),
            _ => Err("Wrong kind of ValueExpr supplied"),
        }
    }
}

impl<'i, 's> LexWith<'i, &'s Scheme> for ValueExprWrapper<'s> {
    fn lex_with(input: &'i str, scheme: &'s Scheme) -> LexResult<'i, Self> {
        // attempt to process the expression as a bitwise operation first
        match BitwiseExpr::lex_with(input, scheme) {
            Ok((expr, rest)) => Ok((Self::BitwiseExpr(expr), rest)),
            Err(_) => {
                IndexExpr::lex_with(input, scheme).map(|(expr, rest)| (Self::IndexExpr(expr), rest))
            }
        }
    }
}

impl<'s> GetType for ValueExprWrapper<'s> {
    fn get_type(&self) -> Type {
        match self {
            ValueExprWrapper::BitwiseExpr(expr) => expr.get_type(),
            ValueExprWrapper::IndexExpr(expr) => expr.get_type(),
        }
    }
}

impl<'s> ValueExpr<'s> for ValueExprWrapper<'s> {
    #[inline]
    fn walk<V: Visitor<'s>>(&self, visitor: &mut V) {
        visitor.visit_index_expr(self.index_expr())
    }

    #[inline]
    fn walk_mut<V: VisitorMut<'s>>(&mut self, visitor: &mut V) {
        visitor.visit_index_expr(self.index_expr_mut())
    }

    fn compile_with_compiler<U: 's, C: Compiler<'s, U> + 's>(
        self,
        compiler: &mut C,
    ) -> CompiledValueExpr<'s, U> {
        match self {
            ValueExprWrapper::BitwiseExpr(expr) => expr.compile_with_compiler(compiler),
            ValueExprWrapper::IndexExpr(expr) => expr.compile_with_compiler(compiler),
        }
    }
}

impl<'s> ValueExprWrapper<'s> {
    pub(crate) fn map_each_count(&self) -> usize {
        match self {
            ValueExprWrapper::BitwiseExpr(_) => 0,
            ValueExprWrapper::IndexExpr(expr) => expr.map_each_count(),
        }
    }

    pub(crate) fn index_expr_mut(&mut self) -> &mut IndexExpr<'s> {
        match self {
            ValueExprWrapper::BitwiseExpr(expr) => &mut expr.lhs,
            ValueExprWrapper::IndexExpr(expr) => expr,
        }
    }

    pub(crate) fn index_expr(&self) -> &IndexExpr<'s> {
        match self {
            ValueExprWrapper::BitwiseExpr(expr) => &expr.lhs,
            ValueExprWrapper::IndexExpr(expr) => expr,
        }
    }

    /// Compiles an [`ValueExprWrapper`] node into a [`CompiledExpr`] (boxed
    /// closure) using the provided comparison function that returns a
    /// boolean.
    pub fn compile_with<F: 's, U: 's, C: Compiler<'s, U> + 's>(
        self,
        compiler: &mut C,
        default: bool,
        func: F,
    ) -> CompiledExpr<'s, U>
    where
        F: Fn(&LhsValue<'_>, &ExecutionContext<'_, U>) -> bool + Sync + Send,
    {
        match self {
            ValueExprWrapper::BitwiseExpr(expr) => expr.compile_with(compiler, default, func),
            ValueExprWrapper::IndexExpr(expr) => expr.compile_with(compiler, default, func),
        }
    }
}
