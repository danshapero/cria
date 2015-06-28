
exception TypeCheckFailure of string
type type_context = DataTypes.t Map.Make(String).t
val empty_context : type_context
val add_binding : Expressions.variable ->
                  DataTypes.t ->
                  type_context ->
                  type_context
val add_variables : (Expressions.variable list) ->
                    DataTypes.t ->
                    type_context ->
                    type_context
val typeof_variable : Expressions.variable ->
                      type_context ->
                      DataTypes.t
val typeof : Expressions.expr -> type_context -> DataTypes.t
val typecheck : (Expressions.expr list) -> type_context -> type_context
