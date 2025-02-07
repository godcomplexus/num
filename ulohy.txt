**Uloha 1**
# Numerické derivace pro f(x) = cos(5 * arccos(x))

# Definujeme funkci f(x)
f <- function(x) {
  return(cos(5 * acos(x)))
}

# Numerická první derivace (centrální diferenciační podíl)
numerical_derivative <- function(f, x, h = 1e-5) {
  return((f(x + h) - f(x - h)) / (2 * h))
}

# Numerická druhá derivace (druhá diferenciační metoda)
numerical_second_derivative <- function(f, x, h = 1e-5) {
  return((f(x + h) - 2 * f(x) + f(x - h)) / (h^2))
}

# Hledání stacionárních bodů (kde f'(x) = 0)
find_stationary_points <- function(f, lower = -1, upper = 1, step = 0.01) {
  x_vals <- seq(lower, upper, by = step)
  deriv_vals <- sapply(x_vals, function(x) numerical_derivative(f, x))
  stationary_points <- x_vals[which(abs(deriv_vals) < 1e-5)]
  return(stationary_points)
}

# Hledání inflexních bodů (kde f''(x) = 0)
find_inflection_points <- function(f, lower = -1, upper = 1, step = 0.01) {
  x_vals <- seq(lower, upper, by = step)
  second_deriv_vals <- sapply(x_vals, function(x) numerical_second_derivative(f, x))
  inflection_points <- x_vals[which(abs(second_deriv_vals) < 1e-5)]
  return(inflection_points)
}

# Interpolace numerických derivací pomocí polynomiální interpolace
interpolate_derivative <- function(x_vals, y_vals, new_x) {
  model <- approx(x_vals, y_vals, xout = new_x, method = "linear")
  return(model$y)
}

# Testovací výpočet
x_test <- seq(-1, 1, length.out = 100)
f_test <- sapply(x_test, f)
f_prime_test <- sapply(x_test, function(x) numerical_derivative(f, x))
f_double_prime_test <- sapply(x_test, function(x) numerical_second_derivative(f, x))

# Interpolace derivací
new_x_test <- seq(-1, 1, length.out = 200)
f_prime_interp <- interpolate_derivative(x_test, f_prime_test, new_x_test)
f_double_prime_interp <- interpolate_derivative(x_test, f_double_prime_test, new_x_test)

# Výpis výsledků
stationary_points <- find_stationary_points(f)
inflection_points <- find_inflection_points(f)

list(
  "Stacionární body" = stationary_points,
  "Inflexní body" = inflection_points,
  "Interpolovaná první derivace" = f_prime_interp,
  "Interpolovaná druhá derivace" = f_double_prime_interp
)
