import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

-- main
main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- model
type alias Task =
  { task : String
  , complete : Bool
  , id : Int
  }

type alias Filter =
  { incomplete : Bool
  , complete : Bool
  }

type alias Model =
  { tasks : List Task
  , new_task : String
  , filters : Filter
  }


init : (Model, Cmd Msg)
init =
  ( Model [] "" (Filter True False), Cmd.none)

-- update
type Msg
  = AddTask
  | TaskInput String
  | DeleteTask Int
  | ToggleTask Int
  | ToggleShowComplete
  | ToggleShowIncomplete

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddTask ->
      ({ model | tasks = model.tasks ++ [ Task model.new_task False (List.length model.tasks) ], new_task = "" }, Cmd.none)
    TaskInput new_input ->
      ({ model | new_task = new_input }, Cmd.none)
    DeleteTask id ->
      ({ model | tasks = List.filter (\t -> t.id /= id) model.tasks }, Cmd.none)
    ToggleTask id ->
      ({ model | tasks = List.map (\t -> if t.id == id then { t | complete = True } else t ) model.tasks }, Cmd.none)
    ToggleShowIncomplete ->
      let
        filters = model.filters
        updatedFilters = { filters | incomplete = not filters.incomplete }
      in
        ({ model | filters = updatedFilters }, Cmd.none)
    ToggleShowComplete ->
      let
        filters = model.filters
        updatedFilters = { filters | complete = not filters.complete }
      in
        ({ model | filters = updatedFilters }, Cmd.none)

-- subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- view
view : Model -> Html Msg
view model =
  div []
    [ input [ onInput TaskInput, Html.Attributes.value model.new_task ] []
    , button [ onClick AddTask ] [ text "Add Task" ]
    , (renderTasks model .incomplete "Tasks" getIncomplete)
    , (renderTasks model .complete "Completed Tasks" getComplete)
    , (renderCheckboxFieldset model)
    ]

renderCheckboxFieldset model =
  Html.fieldset []
        [ Html.label [] [ Html.input [ Html.Attributes.type_ "checkbox", onClick ToggleShowComplete ] [],  Html.text "Show Completed Tasks" ]
        , Html.label [] [ Html.input [ Html.Attributes.type_ "checkbox", checked model.filters.incomplete, onClick ToggleShowIncomplete ] [],  Html.text "Show incomplete Tasks" ]
        ]

renderTasks model checker title filter =
  if checker model.filters then
    div []
    [ div [] [ text title ]
    , ul [] (model.tasks |> filter |> List.map taskToLi)
    ]
  else
    div [] []

getComplete : List Task -> List Task
getComplete tasks =
  List.filter (\t -> t.complete) tasks

getIncomplete : List Task -> List Task
getIncomplete tasks =
  List.filter (\t -> not t.complete) tasks

taskToLi : Task -> Html Msg
taskToLi t =
  li []
    [ text t.task
    , button [ onClick (DeleteTask t.id) ] [ text "x" ]
    , button [ onClick (ToggleTask t.id) ] [ text "done" ]
    ]
