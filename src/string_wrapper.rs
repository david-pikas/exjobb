use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone, Debug)]
/// We have statically known names for things (like bool) as well as dynamically generated ones
/// This type allows using them interchangeably and cloning without actually copying the strings
pub enum StringWrapper {
    Static(&'static str),
    Dynamic(Rc<String>)
}

impl Hash for StringWrapper {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl PartialEq<StringWrapper> for StringWrapper {
    fn eq(&self, other: &StringWrapper) -> bool {
        match (self, other) {
            (StringWrapper::Static(s1), StringWrapper::Static(s2)) => s1 == s2,
            (StringWrapper::Static(s1), StringWrapper::Dynamic(s2)) => s1 == &s2.as_ref().as_str(),
            (StringWrapper::Dynamic(s1), StringWrapper::Static(s2)) => &s1.as_ref().as_str() == s2,
            (StringWrapper::Dynamic(s1), StringWrapper::Dynamic(s2)) => s1 == s2
        }
    }
}
impl Eq for StringWrapper {}
impl ToString for StringWrapper {
    fn to_string(&self) -> String {
        match self {
            StringWrapper::Static(s) => s.to_string(),
            StringWrapper::Dynamic(s) => String::clone(s)
        }
    }
}

impl StringWrapper {
    pub fn as_str<'a>(&'a self) -> &'a str {
        match self {
            StringWrapper::Static(s) => s,
            StringWrapper::Dynamic(s) => &*s
        }
    }
    pub fn starts_with(&self, p: &str) -> bool {
        self.as_str().starts_with(p)
    }
}

impl PartialEq<String> for StringWrapper {
    fn eq(&self, other: &String) -> bool {
        match self {
            StringWrapper::Static(s) => s == &other.as_str(),
            StringWrapper::Dynamic(s) => s.as_ref() == other
        }
    }
}


impl PartialEq<&str> for StringWrapper {
    fn eq(&self, other: &&str) -> bool {
        match self {
            StringWrapper::Static(s) => s == other,
            StringWrapper::Dynamic(s) => &s.as_ref().as_str() == other
        }
    }
}

impl From<String> for StringWrapper {
    fn from(s: String) -> Self {
        StringWrapper::Dynamic(Rc::new(s))
    }
}

impl From<&'static str> for StringWrapper {
    fn from(s: &'static str) -> Self {
        StringWrapper::Static(s)
    }
}
