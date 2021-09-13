use std::backtrace::Backtrace;

use arbitrary::Unstructured;
use crate::context_arbitrary::{Result, GenerationError};

// Verision of Unstructured::choose that doesn't borrow it's argument
// Modified from: https://docs.rs/arbitrary/0.4.7/src/arbitrary/unstructured.rs.html#366-373
pub fn choose_consume<'a, T, I>(u: &mut Unstructured<'a>, mut choices: I) -> Result<T> 
where I: ExactSizeIterator<Item = T> {
    if choices.len() == 0 {
        return Err(GenerationError::NoChoicesError(Backtrace::capture()))
    }
    let idx = u.int_in_range(0..=choices.len() - 1)?;
    Ok(choices.nth(idx).unwrap())
}

pub fn shuffle<T>(u: &mut Unstructured, v: &mut Vec<T>) -> Result<()> {
    //! Fisher-Yates shuffle
    for i in 0..v.len() {
        let j = choose_consume(u, (0..=i as u16).into_iter())?;
        unsafe {
            // Usually aliasing is not allowed, but in this case swapping
            // a value with itself is perfectly valid which is why we must use
            // ptr::swap. The function is safe as long as the pointers are
            // aligned and valid which they trivially are in this case.
            std::ptr::swap(&mut v[i], &mut v[j as usize]);
        }
    }
    Ok(())
}

pub fn interleave<T, I1, I2>(u: &mut Unstructured, mut i1: I1, mut i2: I2) -> Result<Vec<T>>
    where I1: ExactSizeIterator<Item=T>, I2: ExactSizeIterator<Item=T> {

    let len1 = i1.len();
    let len2 = i2.len();

    let mut result = vec![];

    while i1.len() != 0 || i2.len() != 0 {
        if i1.len() == 0 { 
            result.push(i2.next().unwrap());
            continue;
        }
        if i2.len() == 0 { 
            result.push(i1.next().unwrap());
            continue;
        }
        let choice = u.int_in_range(0..=len1+len2)?;
        if choice < len1 {
            result.push(i1.next().unwrap());
        } else {
            result.push(i2.next().unwrap());
        }
    }

    Ok(result)

}

#[macro_export]
macro_rules! guarded_lazy_choose {
    ($u:ident, { $($guard:expr => $choice:expr),*$(,)? }) => {
        (|| {
            let mut len = 0;
            let mut conds = vec![];
            $( 
                conds.push($guard);
                if *conds.last().unwrap() {
                    len += 1;
                }
            )*
            let choice = $u.int_in_range(0..=(len-1))?;
            let mut clause = 0;
            let mut i = 0;
            $(
                if conds[i] && choice == clause {
                    return Ok($choice); 
                }
                if conds[i] {
                    clause += 1;
                }
                i += 1;
            )*
            // purely to supress warnings about the variables being set but never read
            std::mem::drop(clause);
            std::mem::drop(i);
            return Err(GenerationError::NoChoicesError(Backtrace::capture()))
        })()
    };
}

#[macro_export]
macro_rules! weighted_lazy_choose {
    ($u:ident, { $($weight:expr => $choice:expr),*$(,)? }) => {
        (|| {
            let mut ranges = vec![];
            let mut sum = 0;

            $({
                let old_sum = sum;
                sum += $weight;
                ranges.push(old_sum..sum)
            })*

            let num = $u.int_in_range(0..=sum-1)?;

            let mut i = 0;
            $(
                if ranges[i].contains(&num) {
                    return Ok($choice);
                }
                i += 1;
            )*
            std::mem::drop(i);
            return Err(GenerationError::NoChoicesError(Backtrace::capture()))
        })()
    }
}

#[macro_export]
macro_rules! lazy_choose {
    ($u:ident, { $($choice:expr),*$(,)? })
    => (guarded_lazy_choose!($u, { $(true => $choice),*}))
}


