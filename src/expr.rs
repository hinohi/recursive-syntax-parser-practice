#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    ZeroDivision,
}

impl Expr {
    pub fn eval(&self) -> Result<f64, RuntimeError> {
        match self {
            Expr::Number(n) => Ok(*n),
            Expr::Add(a, b) => Ok(a.eval()? + b.eval()?),
            Expr::Sub(a, b) => Ok(a.eval()? - b.eval()?),
            Expr::Mul(a, b) => Ok(a.eval()? * b.eval()?),
            Expr::Div(a, b) => {
                let a = a.eval()?;
                let b = b.eval()?;
                if b == 0.0 {
                    Err(RuntimeError::ZeroDivision)
                } else {
                    Ok(a / b)
                }
            }
            Expr::Pow(a, b) => Ok(a.eval()?.powf(b.eval()?)),
        }
    }
}
