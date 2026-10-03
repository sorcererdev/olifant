pub enum Failure { Unknown(String), NotFound, Locked { by: String }, A, B, C, D }

pub fn status(f: &Failure) -> u16 {
    match f {
        Failure::Unknown(_) => 500,
        Failure::NotFound => 404
    }
}

pub fn one(f: &Failure) -> u16 {
    match f {
        Failure::Unknown(_) => 500,
        Failure::NotFound => 404,
        Failure::Locked { .. } => 423,
        Failure::A | Failure::B | Failure::C => 1,
    }
}

pub fn block(f: Failure) -> u16 {
    match f {
        Failure::Unknown(_) | Failure::NotFound | Failure::Locked { .. } | Failure::A | Failure::B | Failure::C => {
            match &f {
                Failure::A => 1,
                _ => 2,
            }
        }
    }
}

pub fn nested(f: &Failure, g: &Failure) -> u16 {
    match f {
        Failure::NotFound => match g {
            Failure::NotFound => 1,
            Failure::Unknown(_) | Failure::Locked { .. } | Failure::A | Failure::B | Failure::C => 2,
        },
        Failure::Unknown(_) | Failure::Locked { .. } | Failure::A | Failure::B | Failure::C => 3,
    }
}

pub fn letp(f: Failure) { let Failure::NotFound = f; }
