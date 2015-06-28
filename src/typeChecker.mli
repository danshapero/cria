
exception TypeCheckFailure of string
type type_context = DataTypes.t Map.Make(String).t
val empty_context : type_context
val add_binding : Var.t ->
                  DataTypes.t ->
                  type_context ->
                  type_context
val add_variables : (Var.t list) ->
                    DataTypes.t ->
                    type_context ->
                    type_context
val typeof_variable : Var.t ->
                      type_context ->
                      DataTypes.t
val typeof : Expr.t -> type_context -> DataTypes.t
val typecheck : (Expr.t list) -> type_context -> type_context
