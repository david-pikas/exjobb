use arbitrary::Arbitrary;
pub use arbitrary::Unstructured;
use std::{error::Error, fmt::Display, marker::PhantomData, backtrace::Backtrace};
pub trait ContextArbitrary<'a, Ctx>: Sized {
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self>;
}

#[derive(Debug)]
pub enum GenerationError {
    ArbitraryError(arbitrary::Error, Backtrace),
    /// If it isn't possible to produce a value of an appropriate type,
    AppropriateTypeFailure(Backtrace),
    NoChoicesError(Backtrace)
}

impl Display for GenerationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use GenerationError::*;
        match self {
            ArbitraryError(e, bt) => { e.fmt(f)?; write!(f, "\n{}", bt) },
            AppropriateTypeFailure(bt)
                => write!(f, "Couldn't find an apropriate type {}", bt),
            NoChoicesError(bt) => write!(f, "No choices available {}", bt),
        }
    }
}

impl Error for GenerationError { }

impl From<arbitrary::Error> for GenerationError {
    fn from(e: arbitrary::Error) -> Self {
        GenerationError::ArbitraryError(e, Backtrace::capture())
    }
}

pub type Result<T> = std::result::Result<T, GenerationError>;

pub struct FiniteF64(pub f64);
impl<'a> Arbitrary<'a> for FiniteF64 {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
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

pub struct ContextArbitraryIter<'a, 'b, 'c, 'd, El, Ctx> {
    gen: Box<dyn FnMut(&mut Ctx, &mut Unstructured<'a>) -> Result<El> + 'd>,
    u: &'b mut Unstructured<'a>,
    ctx: &'c mut Ctx,
    max_len: usize,
    index: usize,
    _marker: PhantomData<El>,
}

impl<'a, 'b, 'c, 'd, El, Ctx> Iterator for ContextArbitraryIter<'a, 'b, 'c, 'd, El, Ctx>
{
    type Item = Result<El>;
    fn next(&mut self) -> Option<Self::Item> {
        self.index += 1;
        let keep_going = self.max_len <= self.index && self.u.arbitrary().unwrap_or(false);
        if keep_going {
            Some((self.gen)(self.ctx, self.u))
        } else {
            None
        }
    }
}

pub struct ContextArbitraryIterNonMut<'a, 'b, 'c, 'd, El, Ctx> {
    gen: Box<dyn FnMut(&Ctx, &mut Unstructured<'a>) -> Result<El> + 'd>,
    u: &'b mut Unstructured<'a>,
    ctx: &'c Ctx,
    max_len: usize,
    index: usize,
    _marker: PhantomData<El>,
}

impl<'a, 'b, 'c, 'd, El, Ctx> Iterator for ContextArbitraryIterNonMut<'a, 'b, 'c, 'd, El, Ctx>
{
    type Item = Result<El>;
    fn next(&mut self) -> Option<Self::Item> {
        self.index += 1;
        let keep_going = self.max_len <= self.index && self.u.arbitrary().unwrap_or(false);
        if keep_going {
            Some((self.gen)(self.ctx, self.u))
        } else {
            None
        }
    }
}

pub fn c_arbitrary_iter<'a, 'b, 'c, 'd, El: 'd , Ctx: 'd>(
    ctx: &'c mut Ctx,
    u: &'b mut Unstructured<'a>,
) -> ContextArbitraryIter<'a, 'b, 'c, 'd, El, Ctx>
where
    El: ContextArbitrary<'a, Ctx>,
{
    c_arbitrary_iter_with(ctx, u, c_arbitrary)
}

pub fn c_arbitrary_iter_with<'a, 'b, 'c, 'd, El, Ctx, F: 'd>(
    ctx: &'c mut Ctx,
    u: &'b mut Unstructured<'a>,
    gen: F
) -> ContextArbitraryIter<'a, 'b, 'c, 'd, El, Ctx>
where
    F: FnMut(&mut Ctx, &mut Unstructured<'a>) -> Result<El>
{
    let max_len = u.int_in_range(0..=5).unwrap_or(1);
    ContextArbitraryIter {
        u,
        ctx,
        max_len,
        gen: Box::new(gen),
        index: 0,
        _marker: PhantomData,
    }
}

pub fn c_arbitrary_iter_with_non_mut<'a, 'b, 'c, 'd, El, Ctx, F: 'd>(
    ctx: &'c Ctx,
    u: &'b mut Unstructured<'a>,
    gen: F
) -> ContextArbitraryIterNonMut<'a, 'b, 'c, 'd, El, Ctx>
where
    F: FnMut(&Ctx, &mut Unstructured<'a>) -> Result<El>
{
    let max_len = u.int_in_range(0..=5).unwrap_or(0);
    ContextArbitraryIterNonMut {
        u,
        ctx,
        max_len,
        gen: Box::new(gen),
        index: 0,
        _marker: PhantomData,
    }
}

pub struct NEVec<T>(pub Vec<T>);
impl<'a, 'd, T, Ctx> ContextArbitrary<'a, Ctx> for NEVec<T>
where
    T: ContextArbitrary<'a, Ctx>,
{
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

impl<'a, Ctx> ContextArbitrary<'a, Ctx> for () {
    fn c_arbitrary(_ctx: &mut Ctx, _u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(())
    }
}

#[inline(always)]
pub fn c_arbitrary<'a, Ctx, T>(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<T>
where
    T: ContextArbitrary<'a, Ctx>,
{
    ContextArbitrary::c_arbitrary(ctx, u)
}

#[allow(dead_code)]
pub fn maybe<'a, T>(u: &mut Unstructured<'a>, a: T) -> Option<T> {
    let choices = [|a| Some(a), |_| None];
    let choice = u.choose(&choices).ok()?;
    return choice(a);
}

#[allow(unused_macros)]
macro_rules! lazy_maybe {
    ($u:expr, $e:expr) => {
        if Arbitrary::arbitrary($u)? {
            Some($e)
        } else {
            None
        }
    }
}

impl<'a, T, Ctx> ContextArbitrary<'a, Ctx> for Vec<T>
where
    T: ContextArbitrary<'a, Ctx>,
{
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self> {
        c_arbitrary_iter(ctx, u).collect()
    }
}

impl<'a, T, Ctx> ContextArbitrary<'a, Ctx> for Box<T>
where
    T: ContextArbitrary<'a, Ctx>,
{
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Box::new(c_arbitrary(ctx, u)?))
    }
}

impl<'a, T, Ctx> ContextArbitrary<'a, Ctx> for Option<T>
where
    T: ContextArbitrary<'a, Ctx>,
{
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [|ctx, u| Ok(Some(c_arbitrary(ctx, u)?)), |_, _| Ok(None)];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}
