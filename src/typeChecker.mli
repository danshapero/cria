
exception TypeCheckFailure of string
type type_context = DataTypes.data_type Map.Make(String).t
val empty_context : type_context
val add_binding : Expressions.variable ->
                  DataTypes.data_type ->
                  type_context ->
                  type_context
val add_variables : (Expressions.variable list) ->
                    DataTypes.data_type ->
                    type_context ->
                    type_context
val typeof_variable : Expressions.variable ->
                      type_context ->
                      DataTypes.data_type
val typeof : Expressions.expr -> type_context -> DataTypes.data_type
val typecheck : (Expressions.expr list) -> type_context -> type_context
