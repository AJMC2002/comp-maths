pub mod monte_carlo;
pub mod rectangle;
pub mod simpson;
pub mod trapezium;

pub use monte_carlo::*;
pub use rectangle::*;
pub use simpson::*;
use strum_macros::EnumIter;
pub use trapezium::*;

use crate::{Function, IntegralResult};

#[derive(EnumIter, Debug)]
pub enum Methods {
    RectLeft,
    RectRight,
    RectCenter,
    Trapezium,
    Simpson,
    MonteCarlo,
}

impl Methods {
    pub fn name(&self) -> String {
        match self {
            Methods::RectLeft => "Left Rect.".to_string(),
            Methods::RectRight => "Right Rect.".to_string(),
            Methods::RectCenter => "Center Rect.".to_string(),
            Methods::Trapezium => "Trapezium".to_string(),
            Methods::Simpson => "Simpson".to_string(),
            Methods::MonteCarlo => "Monte Carlo".to_string(),
        }
    }

    pub fn method(&self) -> fn(&Function) -> IntegralResult {
        match self {
            Methods::RectLeft => rect_left,
            Methods::RectRight => rect_right,
            Methods::RectCenter => rect_center,
            Methods::Trapezium => trapezium,
            Methods::Simpson => simpson,
            Methods::MonteCarlo => monte_carlo,
        }
    }
}
