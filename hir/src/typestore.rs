use std::collections::HashMap;
use super::typedef::Type;


pub trait TypeStore {
  fn get(&self, type_id: String) -> Option<Type>;
  fn change(&mut self, type_id: String, to_type: Type);
}

pub struct BasicTypeStore {
  map: HashMap<String, Type>
}

impl BasicTypeStore {
  pub fn new() -> Self {
      BasicTypeStore { map: HashMap::new() }
  }
}

impl TypeStore for BasicTypeStore {
  fn get(&self, type_id: String) -> Option<Type> {
      self.map.get(&type_id).cloned()
  }
  fn change(&mut self, type_id: String, to_type: Type) {
    self.map.insert(type_id, to_type);
  }
}