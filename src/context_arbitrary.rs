use arbitrary::Arbitrary;
pub use arbitrary::{Result, Unstructured};
use std::marker::PhantomData;
pub trait ContextArbitrary<'a, Ctx>: Sized {
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self>;
}

pub struct FiniteF64(pub f64);
impl<'a> Arbitrary<'a> for FiniteF64 {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let mut data: f64 = Arbitrary::arbitrary(u)?;
        while !data.is_finite() {
            data = Arbitrary::arbitrary(u)?;
        }
        return Ok(FiniteF64(data));
    }
}
#[allow(dead_code)]
pub fn unwrap_finite_f64(FiniteF64(f64): FiniteF64) -> f64 {
   f64
}

pub struct ContextArbitraryIter<'a, 'b, 'c, El: ContextArbitrary<'a, Ctx>, Ctx> {
    u: &'b mut Unstructured<'a>,
    ctx: &'c mut Ctx,
    max_len: usize,
    index: usize,
    _marker: PhantomData<El>
}

impl<'a, 'b, 'c, El, Ctx> Iterator for ContextArbitraryIter<'a, 'b, 'c, El, Ctx>
where El: ContextArbitrary<'a, Ctx> {
    type Item = Result<El>;
    fn next(&mut self) -> Option<Self::Item> {
        self.index += 1;
        let keep_going = self.max_len <= self.index
                      && self.u.arbitrary().unwrap_or(false);
        if keep_going {
            Some(c_arbitrary(self.ctx, self.u))
        } else {
            None
        }
    }
}

pub fn c_arbitrary_iter<'a, 'b, 'c, El, Ctx>(ctx: &'c mut Ctx, u: &'b mut Unstructured<'a>)
   -> ContextArbitraryIter<'a, 'b, 'c, El, Ctx>
   where El: ContextArbitrary<'a, Ctx> {
    let max_len = u.int_in_range(0..=5).unwrap();
    ContextArbitraryIter {
        u, ctx, max_len, index: 0, _marker: PhantomData
    }
}

pub struct NEVec<T>(pub Vec<T>);
impl<'a, T, Ctx> ContextArbitrary<'a, Ctx> for NEVec<T> 
where T: ContextArbitrary<'a, Ctx> {
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self> {
        let first: T = c_arbitrary(ctx, u)?;
        let rest: ContextArbitraryIter<T, Ctx> = c_arbitrary_iter(ctx, u);
        let iter = std::iter::once(Ok(first)).chain(rest);
        return Ok(NEVec(iter.collect::<Result<Vec<T>>>()?));
    }
}

#[allow(dead_code)]
pub fn unwrap_nev<T>(NEVec(contents): NEVec<T>) -> Vec<T> {
   contents
}

#[inline(always)]
pub fn c_arbitrary<'a, Ctx, T>(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<T>
where T: ContextArbitrary<'a, Ctx> {
   ContextArbitrary::c_arbitrary(ctx, u)
}

#[allow(dead_code)]
pub fn maybe<'a, T>(u: &mut Unstructured<'a>, a: T) -> Option<T> {
     let choices = [|a| Some(a), |_| None];
     let choice = u.choose(&choices).ok()?;
     return choice(a);
}

impl<'a, T, Ctx> ContextArbitrary<'a, Ctx> for Vec<T> 
where T: ContextArbitrary<'a, Ctx> {
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self> {
        c_arbitrary_iter(ctx, u).collect()
    }
}

impl<'a, T, Ctx> ContextArbitrary<'a, Ctx> for Box<T> 
where T: ContextArbitrary<'a, Ctx> {
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Box::new(c_arbitrary(ctx, u)?))
    }
}

impl<'a, T, Ctx> ContextArbitrary<'a, Ctx> for Option<T> 
where T: ContextArbitrary<'a, Ctx> {
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            |ctx, u| Ok(Some(c_arbitrary(ctx, u)?)) ,
            |_  , _| Ok(None) 
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}


