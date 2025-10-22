use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreeValuedLogic {
    True,
    False,
    Unknown,
}

impl BitAnd for ThreeValuedLogic {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ThreeValuedLogic::False, _) | (_, ThreeValuedLogic::False) => ThreeValuedLogic::False,
            (ThreeValuedLogic::True, ThreeValuedLogic::True) => ThreeValuedLogic::True,
            _ => ThreeValuedLogic::Unknown,
        }
    }
}

impl BitAndAssign for ThreeValuedLogic {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl BitOr for ThreeValuedLogic {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ThreeValuedLogic::True, _) | (_, ThreeValuedLogic::True) => ThreeValuedLogic::True,
            (ThreeValuedLogic::False, ThreeValuedLogic::False) => ThreeValuedLogic::False,
            _ => ThreeValuedLogic::Unknown,
        }
    }
}

impl BitOrAssign for ThreeValuedLogic {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl ThreeValuedLogic {
    pub fn is_true(&self) -> bool {
        matches!(self, ThreeValuedLogic::True)
    }

    pub fn is_false(&self) -> bool {
        matches!(self, ThreeValuedLogic::False)
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, ThreeValuedLogic::Unknown)
    }
}
