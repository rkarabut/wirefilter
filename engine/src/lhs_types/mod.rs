mod array;
mod map;

use crate::types::LhsValue;

pub use self::{
    array::{Array, ArrayIterator},
    map::{Map, MapIter, MapValuesIntoIter},
};

pub struct AsRefIterator<'a, T: Iterator<Item = &'a LhsValue<'a>>>(T);

impl<'a, T: Iterator<Item = &'a LhsValue<'a>>> AsRefIterator<'a, T> {
    pub fn new(iter: T) -> Self {
        Self(iter)
    }
}

impl<'a, T: Iterator<Item = &'a LhsValue<'a>>> Iterator for AsRefIterator<'a, T> {
    type Item = LhsValue<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(LhsValue::as_ref)
    }
}
