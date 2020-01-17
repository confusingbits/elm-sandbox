module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--
-- import Random.List exposing (list)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onInput)
import List exposing (foldr)
import Random
import Random.Extra exposing (bool, combine, sequence)
import Set exposing (Set)
import Time


flatten2D : List (List a) -> List a
flatten2D list =
    foldr (++) [] list



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { grid : Maybe Grid
    , height : Int
    , width : Int
    }


type alias Cell =
    { x : Int
    , y : Int
    , isAlive : Bool
    }


type alias Grid =
    List Cell


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = Nothing
      , height = 10
      , width = 10
      }
    , Cmd.none
    )


initGrid : Int -> Int -> Grid
initGrid height width =
    let
        h =
            List.range 0 (height - 1)

        w =
            List.range 0 (width - 1)

        grid =
            h |> List.map (\y -> w |> List.map (\x -> Cell x y False)) |> flatten2D
    in
    grid



-- UPDATE


type Msg
    = GotGrid Grid
    | GetGrid
    | NextGeneration Time.Posix
    | Next
    | InputHeight String
    | InputWidth String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputHeight str ->
            let
                m =
                    String.toInt str
            in
            case m of
                Nothing ->
                    ( model, Cmd.none )

                Just i ->
                    ( { model | height = i }, Cmd.none )

        InputWidth str ->
            let
                m =
                    String.toInt str
            in
            case m of
                Nothing ->
                    ( model, Cmd.none )

                Just i ->
                    ( { model | width = i }, Cmd.none )

        GotGrid g ->
            ( { model | grid = Just g }, Cmd.none )

        GetGrid ->
            ( model, generate model.height model.width )

        NextGeneration _ ->
            case model.grid of
                Just g ->
                    ( { model | grid = Maybe.Just (createNextGeneration g) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Next ->
            case model.grid of
                Just g ->
                    ( { model | grid = Maybe.Just (createNextGeneration g) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput InputHeight, value (String.fromInt model.height) ] []
        , input [ onInput InputWidth, value (String.fromInt model.width) ] []
        , button [ class "generate-btn", onClick GetGrid ] [ text "Generate" ]
        , button [ class "generate-btn", onClick Next ] [ text "Next" ]
        , viewGrid model.grid
        ]


viewGrid : Maybe Grid -> Html Msg
viewGrid grid =
    case grid of
        Nothing ->
            div [] []

        Just g ->
            let
                cols =
                    g |> List.map (\c -> c.x) |> Set.fromList |> Set.toList
            in
            div [ class "grid" ] (cols |> List.map (viewRow g))


viewRow : Grid -> Int -> Html Msg
viewRow grid colNum =
    let
        cells =
            grid |> List.filter (\c -> c.x == colNum)
    in
    div [ class "row" ] (List.map viewCell cells)


viewCell : Cell -> Html Msg
viewCell cell =
    let
        pos =
            String.fromInt cell.x ++ "," ++ String.fromInt cell.y
    in
    if cell.isAlive == True then
        div [ class "cell alive" ] []

    else
        div [ class "cell dead" ] []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 NextGeneration


generate : Int -> Int -> Cmd Msg
generate h w =
    Random.generate GotGrid (generateGrid (initGrid h w))


generateGrid : Grid -> Random.Generator Grid
generateGrid grid =
    combine (generateCells grid)


generateCells : List Cell -> List (Random.Generator Cell)
generateCells list =
    list |> List.map generateCell


generateCell : Cell -> Random.Generator Cell
generateCell cell =
    Random.map3 Cell (generateConstant cell.x) (generateConstant cell.y) bool


generateBool : Random.Generator Bool
generateBool =
    bool


generateConstant : Int -> Random.Generator Int
generateConstant a =
    Random.constant a


createNextGeneration : Grid -> Grid
createNextGeneration grid =
    let
        newGrid =
            grid
                |> List.map
                    (\c ->
                        let
                            -- count neighbors
                            count =
                                countNeighbors c.x c.y grid

                            isAlive =
                                liveOrDie count
                        in
                        -- handle neighbor count
                        Cell c.x c.y isAlive
                    )
    in
    newGrid


countNeighbors : Int -> Int -> Grid -> Int
countNeighbors x y grid =
    let
        maxx =
            grid
                |> List.sortBy .x
                |> List.reverse
                |> List.head
                |> Maybe.withDefault (Cell 0 0 False)
                |> .x

        maxy =
            grid
                |> List.sortBy .y
                |> List.reverse
                |> List.head
                |> Maybe.withDefault (Cell 0 0 False)
                |> .y

        topLeft =
            getCell grid (-1 + x + maxx |> modBy maxx) (-1 + y + maxy |> modBy maxy)
                |> (\c ->
                        if c.isAlive then
                            1

                        else
                            0
                   )

        topCenter =
            getCell grid (0 + x + maxx |> modBy maxx) (-1 + y + maxy |> modBy maxy)
                |> (\c ->
                        if c.isAlive then
                            1

                        else
                            0
                   )

        topRight =
            getCell grid (1 + x + maxx |> modBy maxx) (-1 + y + maxy |> modBy maxy)
                |> (\c ->
                        if c.isAlive then
                            1

                        else
                            0
                   )

        middleLeft =
            getCell grid (-1 + x + maxx |> modBy maxx) (0 + y + maxy |> modBy maxy)
                |> (\c ->
                        if c.isAlive then
                            1

                        else
                            0
                   )

        center =
            getCell grid (0 + x + maxx |> modBy maxx) (0 + y + maxy |> modBy maxy)
                |> (\c ->
                        if c.isAlive then
                            1

                        else
                            0
                   )

        middleRight =
            getCell grid (1 + x + maxx |> modBy maxx) (0 + y + maxy |> modBy maxy)
                |> (\c ->
                        if c.isAlive then
                            1

                        else
                            0
                   )

        bottomLeft =
            getCell grid (-1 + x + maxx |> modBy maxx) (1 + y + maxy |> modBy maxy)
                |> (\c ->
                        if c.isAlive then
                            1

                        else
                            0
                   )

        bottomCenter =
            getCell grid (0 + x + maxx |> modBy maxx) (1 + y + maxy |> modBy maxy)
                |> (\c ->
                        if c.isAlive then
                            1

                        else
                            0
                   )

        bottomRight =
            getCell grid (1 + x + maxx |> modBy maxx) (1 + y + maxy |> modBy maxy)
                |> (\c ->
                        if c.isAlive then
                            1

                        else
                            0
                   )

        count =
            topLeft
                + topCenter
                + topRight
                + middleLeft
                + center
                + middleRight
                + bottomLeft
                + bottomCenter
                + bottomRight
    in
    count


getCell : Grid -> Int -> Int -> Cell
getCell grid x y =
    grid
        |> List.filter (\r -> r.x == x && r.y == y)
        |> List.head
        |> Maybe.withDefault (Cell 0 0 False)


liveOrDie : Int -> Bool
liveOrDie num =
    if num > 3 then
        False

    else if num < 2 then
        False

    else
        True
