
exception TypeCheckFailure of string
type type_context = DataTypes.t Map.Make(String).t
val empty_context : type_context
val add_binding : Expressions.var ->
                  DataTypes.t ->
                  type_context ->
                  type_context
val add_variables : (Expressions.var list) ->
                    DataTypes.t ->
                    type_context ->
                    type_context
val typeof_variable : Expressions.var ->
                      type_context ->
                      DataTypes.t
val typeof : Expressions.t -> type_context -> DataTypes.t
val typecheck : (Expressions.t list) -> type_context -> type_context
