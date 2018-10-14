# Package independent support for validating parameters ---

parameter_validation_messages <- list(
  cannot_be_null =
    "The parameter cannot be null. Parameter name: {1}.",
  must_be_function =
    "The parameter must be a function. Parameter name: {1}.",
  must_be_integer =
    "The parameter must be integer. Parameter name: {1}.",
    must_be_logical =
    "The parameter must be logical. Parameter name: {1}.",
must_be_in_range =
    "The parameter must be in range [{2}, {3}]. Parameter name: {1}.",
  must_be_matrix =
    "The parameter must be a matrix. Parameter name: {1}.",
  must_be_non_negative =
    "The parameter must be non negative. Parameter name: {1}.",
  must_be_numeric =
    "The parameter must be numeric. Parameter name: {1}.",
  must_be_positive =
    "The parameter must be positive. Parameter name: {1}.",
  must_be_orthogonal =
    "The parameter must be an orthogonal matrix. Parameter name: {1}.",
  must_be_scalar =
    "The parameter must be scalar. Parameter name: {1}.",
  must_be_square =
    "The parameter must be a square matrix. Parameter name: {1}.",
  wrong_minimum_number_of_rows =
    "The parameter must have at least {2} rows. Parameter name: {1}.",
  wrong_number_of_columns =
    "Wrong number of columns in parameter {1}. Expected: {2}.",
  wrong_shared_number_of_rows =
    "Parameter {1} must have the same number of rows of parameter {2}.",
  wrong_weights_number_of_columns =
    "Parameter {1} must have a number of columns equal to the number of rows in parameter {2}.",
  wrong_length =
    "Wrong length of parameter {1}. Expected: {2}."
)

ParameterValidator <- setRefClass("ParameterValidator",
  contains = "Validator",
  fields = list(
    name = "character"
  ),
  methods = list(
    check_is_function = function(value, which) {
      base_validate(
        !is.function(value),
        message = base_format_message(
          formatting = parameter_validation_messages$must_be_function,
          name
        ),
        which = which
      )
    },
    check_is_in_range = function(value, lower_bound, upper_bound, which) {
      base_validate(
        !((lower_bound <= value) && value <= upper_bound),
        message = base_format_message(
          formatting = parameter_validation_messages$must_be_in_range,
          name,
          lower_bound,
          upper_bound
        ),
        which = which
      )
    },
    check_is_integer = function(value, which) {
      base_validate(
        !(is.integer(value) || value == as.integer(value)),
        message = base_format_message(
          formatting = parameter_validation_messages$must_be_integer,
          name
        ),
        which = which
      )
    },
    check_is_logical = function(value, which) {
      base_validate(
        !(is.logical(value)),
        message = base_format_message(
          formatting = parameter_validation_messages$must_be_logical,
          name
        ),
        which = which
      )
    },
    check_is_matrix = function(value, which) {
      base_validate(
        !is.matrix(value),
        message = base_format_message(
          formatting = parameter_validation_messages$must_be_matrix,
          name
        ),
        which = which
      )
    },
    check_is_non_negative = function(value, which) {
      base_validate(
      !(value >= 0),
      message = base_format_message(
        formatting = parameter_validation_messages$must_be_non_negative,
        name
      ),
      which = which
      )
    },
    check_is_numeric = function(value, which) {
      base_validate(
        !is.numeric(value),
        message = base_format_message(
          formatting = parameter_validation_messages$must_be_numeric,
          name
        ),
        which = which
      )
    },
    check_is_orthogonal = function(value, which) {
      base_validate(
        tryCatch(
          !all.equal(t(value), solve(value)),
          error = function(c) TRUE
        ),
        message = base_format_message(
          formatting = parameter_validation_messages$must_be_orthogonal,
          name
        ),
        which = which
      )
    },
    check_is_positive = function(value, which) {
      base_validate(
        !(value > 0),
        message = base_format_message(
          formatting = parameter_validation_messages$must_be_positive,
          name
        ),
        which = which
      )
    },
    check_is_scalar = function(value, which) {
      base_validate(
        !(length(value) == 1),
        message = base_format_message(
          formatting = parameter_validation_messages$must_be_scalar,
          name
        ),
        which = which
      )
    },
    check_is_square = function(value, which) {
      base_validate(
        !(nrow(value) == ncol(value)),
        message = base_format_message(
          formatting = parameter_validation_messages$must_be_square,
          name
        ),
        which = which
      )
    },
    check_length = function(
      value,
      expected_length,
      which) {
      base_validate(
        length(value) != expected_length,
        message = base_format_message(
          formatting = parameter_validation_messages$wrong_length,
          name,
          expected_length
        ),
        which = which
      )
    },
    check_minimum_number_of_rows = function(
      value,
      expected_minimum_number_of_rows,
      which) {
      base_validate(
        nrow(value) < expected_minimum_number_of_rows,
        message = base_format_message(
          formatting = parameter_validation_messages$wrong_minimum_number_of_rows,
          name,
          expected_minimum_number_of_rows
        ),
        which = which
      )
    },
    check_number_of_columns = function(
      value,
      expected_number_of_columns,
      which) {
      base_validate(
        ncol(value) != expected_number_of_columns,
        message = base_format_message(
          formatting = parameter_validation_messages$wrong_number_of_columns,
          name,
          expected_number_of_columns
        ),
        which = which
      )
    },
    check_shared_number_of_rows = function(
      value,
      refererred_param,
      refererred_param_name,
      which) {
      base_validate(
        nrow(value) != nrow(refererred_param),
        message = base_format_message(
          formatting = parameter_validation_messages$wrong_shared_number_of_rows,
          name,
          refererred_param_name
        ),
        which = which
      )
    },
    check_weights_number_of_columns = function(
      value,
      refererred_param,
      refererred_param_name,
      which) {
      base_validate(
        ncol(value) != nrow(refererred_param),
        message = base_format_message(
          formatting = parameter_validation_messages$wrong_weights_number_of_columns,
          name,
          refererred_param_name
        ),
        which = which
      )
    },
    stop_if_null = function(value, which) {
      base_validate(
        is.null(value),
        message = base_format_message(
          formatting = parameter_validation_messages$cannot_be_null,
          name
        ),
        which = which
      )
    },
    validate = function(value, which) {
    }
  )
)

# Support for package specific parameters ---

AllowReflectionsValidator <- setRefClass("AllowReflectionsValidator",
  contains = "ParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_logical(value, which)
      check_is_scalar(value, which)
    }
  )
)

ConcentrationValidator <- setRefClass("ConcentrationValidator",
  contains = "ParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_numeric(value, which)
      check_is_scalar(value, which)
      check_is_non_negative(value, which)
    }
  )
)

ConcentrationUpperBoundValidator <- setRefClass("ConcentrationUpperBoundValidator",
  contains = "ParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_numeric(value, which)
      check_is_scalar(value, which)
      check_is_positive(value, which)
    }
  )
)

CartesianPointsValidator <- setRefClass("CartesianPointsValidator",
  contains = "ParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_numeric(value, which)
      check_is_matrix(value, which)
      check_number_of_columns(value, expected_number_of_columns = 3, which)
    }
  )
)

IndependentComponentsValidator <- setRefClass("IndependentComponentsValidator",
  contains = "ParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_numeric(value, which)
      check_length(value, 3, which)
    }
  )
)

PositiveIntegerValidator <- setRefClass("PositiveIntegerValidator",
  contains = "ParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_numeric(value, which)
      check_is_scalar(value, which)
      check_is_integer(value, which)
      check_is_positive(value, which)
    }
  )
)

InRangeIntegerValidator <- setRefClass("InRangeIntegerValidator",
  contains = "ParameterValidator",
  fields = list(
    lower_bound = "numeric",
    upper_bound = "numeric"
  ),
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_numeric(value, which)
      check_is_scalar(value, which)
      check_is_integer(value, which)
      check_is_in_range(value, lower_bound, upper_bound, which)
    }
  )
)

NonNegativeIntegerValidator <- setRefClass("NonNegativeIntegerValidator",
  contains = "ParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_numeric(value, which)
      check_is_scalar(value, which)
      check_is_integer(value, which)
      check_is_non_negative(value, which)
    }
  )
)

RotationMatrixValidator <- setRefClass("RotationMatrixValidator",
  contains = "ParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_numeric(value, which)
      check_is_matrix(value, which)
      check_is_square(value, which)
      check_number_of_columns(value, expected_number_of_columns = 3, which)
      check_is_orthogonal(value, which)
    }
  )
)

SphericalPointsValidator <- setRefClass("SphericalPointsValidator",
  contains = "ParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_numeric(value, which)
      check_is_matrix(value, which)
      check_number_of_columns(value, expected_number_of_columns = 2, which)
    }
  )
)

WeightsValidator <- setRefClass("WeightsValidator",
  contains = "ParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_numeric(value, which)
      check_is_matrix(value, which)
    }
  )
)

parameter_validators <- list(
  allow_reflections =
    AllowReflectionsValidator$new(name = "allow_reflections"),
  cartesian_coords =
    CartesianPointsValidator$new(name = "cartesian_coords"),
  concentration =
    ConcentrationValidator$new(name = "concentration"),
  concentration_upper_bound =
    ConcentrationUpperBoundValidator$new(name = "concentration_upper_bound"),
  evaluation_points =
    CartesianPointsValidator$new(name = "evaluation_points"),
  explanatory_points =
    CartesianPointsValidator$new(name = "explanatory_points"),
  explanatory_point_weights =
    WeightsValidator$new(name = "explanatory_point_weights"),
  response_points =
    CartesianPointsValidator$new(name = "response_points"),
  independent_components =
    IndependentComponentsValidator$new(name = "independent_components"),
  number_of_iterations =
    PositiveIntegerValidator$new(name = "number_of_iterations"),
  number_of_expansion_terms =
    InRangeIntegerValidator$new(
      name = "number_of_expansion_terms",
      lower_bound = 1, upper_bound = 2),
  number_of_points =
    PositiveIntegerValidator$new(name = "number_of_points"),
  rotation_matrix =
    RotationMatrixValidator$new(name = "rotation_matrix"),
  spherical_coords =
    SphericalPointsValidator$new(name = "spherical_coords")
)

# Package independent support for validating parameters whose type is function ---

fnc_typed_parameter_validation_messages <- list(
  cannot_return_null =
    "Parameter {1} is a function returning unexpected NULL.",
  cannot_return_non_matrix =
    "Parameter {1} is a function returning unexpected non matrix value.",
  cannot_return_non_numeric =
    "Parameter {1} is a function returning unexpected non numeric value.",
  missing_function_param =
    "Parameter {1} is a function having a missing argument. Expected argument: {2}.",
  wrong_number_of_function_params =
    "Parameter {1} is a function having the wrong number of parameters. Expected: {2}.",
  wrong_weights_return_value_dimensions =
    paste0("Parameter {1} is a function returning a value having the wrong dimensions. ",
    "Expected: as many rows as the number of explanatory points and ",
    "as many columns as the number of evaluation points."),
  wrong_return_value_length =
    "Parameter {1} is a function returning a value having the wrong length. Expected: {2}."
)

FunctionTypedParameterValidator <- setRefClass("FunctionTypedParameterValidator",
  contains = "ParameterValidator",
  fields = list(
    expected_parameters = "list" # A named list, with names corresponding to expected parameter names
  ),
  methods = list(
    check_params = function(value, which) {
      f <- formals(value)
      number_of_expected_params <- length(expected_parameters)
      base_validate(
        length(f) != number_of_expected_params,
        message = base_format_message(
          formatting = fnc_typed_parameter_validation_messages$wrong_number_of_function_params,
          name,
          number_of_expected_params
        ),
        which = which
      )
      actual_param_names <- names(f)
      expected_param_names <- names(expected_parameters)
      for (i in 1:number_of_expected_params) {
        expected_param <- expected_param_names[i]
        base_validate(
          !is.element(expected_param, actual_param_names),
          message = base_format_message(
            formatting = fnc_typed_parameter_validation_messages$missing_function_param,
            name,
            expected_param
          ),
          which = which
        )
      }
    },
    check_weights_return_value_dimensions = function(
      value,
      which) {
      expected_number_of_rows <- nrow(expected_parameters$explanatory_points)
      expected_number_of_columns <- nrow(expected_parameters$evaluation_points)
      base_validate({
        return_value <- do.call(value, expected_parameters)
        (nrow(return_value) != expected_number_of_rows) || (ncol(return_value) != expected_number_of_columns)
        },
        message = base_format_message(
          formatting = fnc_typed_parameter_validation_messages$wrong_weights_return_value_dimensions,
          name
        ),
        which = which
      )
    },
    check_return_value_is_matrix = function(
      value,
      which) {
      base_validate(
        !is.matrix(do.call(value, expected_parameters)),
        message = base_format_message(
          formatting = fnc_typed_parameter_validation_messages$cannot_return_non_matrix,
          name
        ),
        which = which
      )
    },
    check_return_value_is_numeric = function(
      value,
      which) {
      base_validate(
        !is.numeric(do.call(value, expected_parameters)),
        message = base_format_message(
          formatting = fnc_typed_parameter_validation_messages$cannot_return_non_numeric,
          name
        ),
        which = which
      )
    },
    check_return_value_length = function(
      value,
      expected_length,
      which) {
      base_validate(
        length(do.call(value, expected_parameters)) != expected_length,
        message = base_format_message(
          formatting = fnc_typed_parameter_validation_messages$wrong_return_value_length,
          name,
          expected_length
        ),
        which = which
      )
    },
    stop_if_return_value_null = function(value, which) {
      base_validate(
        is.null(do.call(value, expected_parameters)),
        message = base_format_message(
          formatting = fnc_typed_parameter_validation_messages$cannot_return_null,
          name
        ),
        which = which
      )
    },
    validate = function(value, which) {
    }
  )
)

# Support for package specific function typed parameters ---

LocalOperator <- setRefClass("LocalOperator",
  contains = "FunctionTypedParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_function(value, which)
      check_params(value, which)
      stop_if_return_value_null(value, which)
      check_return_value_is_numeric(value, which)
      check_return_value_length(value, 3, which)
    }
  )
)

WeightsGenerator <- setRefClass("WeightsGenerator",
  contains = "FunctionTypedParameterValidator",
  methods = list(
    validate = function(value, which) {
      stop_if_null(value, which)
      check_is_function(value, which)
      check_params(value, which)
      stop_if_return_value_null(value, which)
      check_return_value_is_numeric(value, which)
      check_return_value_is_matrix(value, which)
      check_weights_return_value_dimensions(value, which)
    }
  )
)

fnc_typed_parameter_validators <- list(
  local_error_sampler =
    LocalOperator$new(
      name = "local_error_sampler",
      expected_parameters = list(
        "point" = matrix(data = c(1.0, 0.0, 0.0), nrow = 1, ncol = 3)
      )
    ),
  local_rotation_composer =
    LocalOperator$new(
      name = "local_rotation_composer",
      expected_parameters = list(
        "point" = matrix(data = c(1.0, 0.0, 0.0), nrow = 1, ncol = 3)
      )
    ),
  weights_generator =
    WeightsGenerator$new(
      name = "weights_generator",
      expected_parameters = list(
        "evaluation_points" = matrix(
          data = c(1.0, 0.0, 0.0, 0.0, 1.0, 0.0),
          nrow = 2,
          ncol = 3),
        "explanatory_points" = matrix(
          data = c(1.0, 0.0, 0.0,
                   0.0, 1.0, 0.0,
                   0.0, 0.0, 1.0,
                   1 / sqrt(2), 0.0, 1 / sqrt(2)),
          nrow = 4,
          ncol = 3),
        "concentration" = 2.0
      )
    )
  )