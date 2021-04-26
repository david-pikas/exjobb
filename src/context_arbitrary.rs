mod arbitrary;

pub trait ContextArbitrary<'a, 'b>[Ctx] {
    fn c_arbitrary(ctx: &mut Ctx<'a>, u: &mut Unstructured<'b>) -> Result<Self>;
}
