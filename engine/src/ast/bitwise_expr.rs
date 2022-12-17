use crate::{
    ast::{
        field_expr::LhsFieldExpr,
        index_expr::{simplify_indexes, IndexExpr},
        CompiledValueExpr, ValueExpr, Visitor, VisitorMut,
    },
    compiler::Compiler,
    execution_context::ExecutionContext,
    filter::{CompiledExpr, CompiledOneExpr},
    lex::{skip_space, span, Lex, LexErrorKind, LexResult, LexWith},
    rhs_types::U256Wrapper,
    scheme::Scheme,
    types::{GetType, LhsValue, Type},
};
use serde::Serialize;

lex_enum!(BitwiseOp {
    "&" | "bitwise_and" => And,
    "|" | "bitwise_or" => Or,
    "^" | "bitwise_xor" => Xor,
    "<<" | "bitwise_shl" => ShiftLeft,
    ">>" | "bitwise_shr" => ShiftRight,
});

/// Bitwise binary expression
#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
pub struct BitwiseExpr<'s> {
    /// Left-hand side of the bitwise binary expression
    pub lhs: IndexExpr<'s>,

    /// Operator + right-hand side of the bitwise binary expression
    #[serde(flatten)]
    pub op: BitwiseOpExpr,
}

impl<'s> GetType for BitwiseExpr<'s> {
    fn get_type(&self) -> Type {
        self.lhs.get_type()
    }
}

impl<'i, 's> LexWith<'i, &'s Scheme> for BitwiseExpr<'s> {
    fn lex_with(input: &'i str, scheme: &'s Scheme) -> LexResult<'i, Self> {
        let (lhs, input) = IndexExpr::lex_with(input, scheme)?;

        Self::lex_with_lhs(input, lhs)
    }
}

// taken from IndexExpr
macro_rules! index_access_one {
    ($indexes:ident, $first:expr, $default:expr, $ctx:ident, $func:expr) => {
        $indexes
            .iter()
            .fold($first, |value, idx| {
                value.and_then(|val| val.get(idx).unwrap())
            })
            .map_or_else(|| $default, |val| $func(val, $ctx))
    };
}

impl<'s> BitwiseExpr<'s> {
    pub(crate) fn lex_with_lhs<'i>(input: &'i str, lhs: IndexExpr<'s>) -> LexResult<'i, Self> {
        let lhs_type = lhs.get_type();

        let initial_input = skip_space(input);
        let (op, input) = BitwiseOp::lex(initial_input)?;

        let input_after_op = input;

        let input = skip_space(input);

        let (op, input) = match &lhs_type {
            Type::Int => {
                let (rhs, input) = i32::lex(input)?;
                (BitwiseOpExpr::Int { op, rhs }, input)
            }
            Type::U256 => {
                let (rhs, input) = U256Wrapper::lex(input)?;
                (BitwiseOpExpr::U256 { op, rhs }, input)
            }
            _ => {
                return Err((
                    LexErrorKind::UnsupportedOp { lhs_type },
                    span(initial_input, input_after_op),
                ));
            }
        };

        Ok((Self { lhs, op }, input))
    }

    /// Compiles a [`BitwiseExpr`] node into a [`CompiledExpr`] (boxed closure)
    /// using the provided comparison function that returns a boolean.
    pub fn compile_with<F: 's, U: 's, C: Compiler<'s, U> + 's>(
        self,
        compiler: &mut C,
        default: bool,
        func: F,
    ) -> CompiledExpr<'s, U>
    where
        F: Fn(&LhsValue<'_>, &ExecutionContext<'_, U>) -> bool + Sync + Send,
    {
        let op_func = Self::op_func(self.op.clone());
        // does _not_ support map_each in the lhs index expr
        let IndexExpr { lhs, indexes } = self.lhs;
        let indexes = simplify_indexes(indexes);
        let one = match lhs {
            LhsFieldExpr::FunctionCallExpr(call) => {
                let call = compiler.compile_function_call_expr(call);
                if indexes.is_empty() {
                    CompiledOneExpr::new(move |ctx| {
                        call.execute(ctx)
                            .map_or(default, |val| func(&op_func(&val), ctx))
                    })
                } else {
                    CompiledOneExpr::new(move |ctx| {
                        index_access_one!(
                            indexes,
                            call.execute(ctx).as_ref().ok(),
                            default,
                            ctx,
                            |v, ctx| { func(&op_func(v), ctx) }
                        )
                    })
                }
            }
            LhsFieldExpr::Field(f) => {
                if indexes.is_empty() {
                    CompiledOneExpr::new(move |ctx| {
                        func(&op_func(ctx.get_field_value_unchecked(f)), ctx)
                    })
                } else {
                    CompiledOneExpr::new(move |ctx| {
                        index_access_one!(
                            indexes,
                            Some(ctx.get_field_value_unchecked(f)),
                            default,
                            ctx,
                            |v, ctx| { func(&op_func(v), ctx) }
                        )
                    })
                }
            }
        };

        CompiledExpr::One(one)
    }

    fn op_func(op: BitwiseOpExpr) -> impl for<'a> Fn(&LhsValue<'a>) -> LhsValue<'a> + 's {
        macro_rules! cast_value {
            ($value:expr, $ty:ident) => {
                match $value {
                    LhsValue::$ty(value) => value,
                    _ => unreachable!(),
                }
            };
        }

        move |val| match op {
            BitwiseOpExpr::Int {
                op: BitwiseOp::And,
                rhs,
            } => (cast_value!(val, Int) & rhs).into(),
            BitwiseOpExpr::Int {
                op: BitwiseOp::Or,
                rhs,
            } => (cast_value!(val, Int) | rhs).into(),
            BitwiseOpExpr::Int {
                op: BitwiseOp::Xor,
                rhs,
            } => (cast_value!(val, Int) ^ rhs).into(),
            BitwiseOpExpr::Int {
                op: BitwiseOp::ShiftLeft,
                rhs,
            } => (cast_value!(val, Int) << rhs).into(),
            BitwiseOpExpr::Int {
                op: BitwiseOp::ShiftRight,
                rhs,
            } => (cast_value!(val, Int) >> rhs).into(),
            BitwiseOpExpr::U256 {
                op: BitwiseOp::And,
                rhs,
            } => U256Wrapper::from(cast_value!(val, U256).value & rhs.value).into(),
            BitwiseOpExpr::U256 {
                op: BitwiseOp::Or,
                rhs,
            } => U256Wrapper::from(cast_value!(val, U256).value | rhs.value).into(),
            BitwiseOpExpr::U256 {
                op: BitwiseOp::Xor,
                rhs,
            } => U256Wrapper::from(cast_value!(val, U256).value ^ rhs.value).into(),
            BitwiseOpExpr::U256 {
                op: BitwiseOp::ShiftLeft,
                rhs,
            } => U256Wrapper::from(cast_value!(val, U256).value << rhs.value.low_u32() as usize)
                .into(),
            BitwiseOpExpr::U256 {
                op: BitwiseOp::ShiftRight,
                rhs,
            } => U256Wrapper::from(cast_value!(val, U256).value >> rhs.value.low_u32() as usize)
                .into(),
        }
    }
}

impl<'s> ValueExpr<'s> for BitwiseExpr<'s> {
    #[inline]
    fn walk<V: Visitor<'s>>(&self, visitor: &mut V) {
        visitor.visit_index_expr(&self.lhs)
    }

    #[inline]
    fn walk_mut<V: VisitorMut<'s>>(&mut self, visitor: &mut V) {
        visitor.visit_index_expr(&mut self.lhs)
    }

    fn compile_with_compiler<U: 's, C: Compiler<'s, U> + 's>(
        self,
        compiler: &mut C,
    ) -> CompiledValueExpr<'s, U> {
        let ty = self.get_type();
        let IndexExpr { lhs, indexes } = self.lhs;
        let op_func = Self::op_func(self.op.clone());

        if indexes.is_empty() {
            match lhs {
                LhsFieldExpr::Field(f) => {
                    CompiledValueExpr::new(move |ctx| Ok(op_func(ctx.get_field_value_unchecked(f))))
                }
                LhsFieldExpr::FunctionCallExpr(call) => {
                    let call = compiler.compile_function_call_expr(call);
                    CompiledValueExpr::new(move |ctx| {
                        let result = call.execute(ctx)?;
                        Ok(op_func(&result))
                    })
                }
            }
        } else {
            match lhs {
                LhsFieldExpr::Field(f) => CompiledValueExpr::new(move |ctx| {
                    indexes
                        .iter()
                        .fold(Some(ctx.get_field_value_unchecked(f)), |value, index| {
                            value.and_then(|val| val.get(index).unwrap())
                        })
                        .map(|v| op_func(v))
                        .ok_or_else(|| ty.clone())
                }),
                LhsFieldExpr::FunctionCallExpr(call) => {
                    let call = compiler.compile_function_call_expr(call);
                    CompiledValueExpr::new(move |ctx| {
                        let result = call.execute(ctx)?;
                        indexes
                            .iter()
                            .fold(Some(result), |value, index| {
                                value.and_then(|val| val.extract(index).unwrap())
                            })
                            .map(|v| op_func(&v))
                            .ok_or_else(|| ty.clone())
                    })
                }
            }
        }
    }
}

/// Operator and right-hand side expression of a
/// bitwise binary expression.
#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
#[serde(untagged)]
pub enum BitwiseOpExpr {
    /// Integer bitwise operations
    Int {
        /// Integer bitwise operator
        op: BitwiseOp,
        /// Right-hand side integer value
        rhs: i32,
    },

    /// 256-bit unsigned integer bitwise operation
    U256 {
        /// Integer bitwise operator
        op: BitwiseOp,
        /// Right-hand side integer value
        rhs: U256Wrapper,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{field_expr::LhsFieldExpr, index_expr::IndexExpr},
        FieldIndex,
    };

    use lazy_static::lazy_static;

    lazy_static! {
        static ref SCHEME: Scheme = {
            let mut scheme = Scheme::new();
            scheme
                .add_field("test".to_string(), Type::Array(Box::new(Type::Int)))
                .unwrap();
            scheme
                .add_field(
                    "test2".to_string(),
                    Type::Array(Box::new(Type::Array(Box::new(Type::U256)))),
                )
                .unwrap();
            scheme
        };
    }

    #[test]
    fn test_bitwise_expr_array_indices() {
        fn run(i: u32) {
            let filter = format!("test[{}] << 1", i);
            assert_ok!(
                BitwiseExpr::lex_with(&filter, &SCHEME),
                BitwiseExpr {
                    lhs: IndexExpr {
                        lhs: LhsFieldExpr::Field(SCHEME.get_field("test").unwrap()),
                        indexes: vec![FieldIndex::ArrayIndex(i)],
                    },
                    op: BitwiseOpExpr::Int {
                        op: BitwiseOp::ShiftLeft,
                        rhs: 1i32,
                    }
                }
            );
        }

        run(0);
        run(1);
        run(99);
        run(999);
        run(9999);
        run(99999);
    }
}
