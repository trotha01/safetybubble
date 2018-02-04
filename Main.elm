module Main exposing (..)

import Animation exposing (..)
import AnimationFrame
import Ease
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import Math.Vector2 exposing (..)
import Mouse
import Random
import Random.Extra as Random
import Svg exposing (..)
import Svg.Attributes exposing (cx, cy, dx, dy, fill, fillOpacity, in_, mode, points, result, rx, ry, stdDeviation, stroke, transform, viewBox, x, xlinkHref, y)
import Task
import Time exposing (..)
import Window


{-| Next Steps

  - Add in collision detection
  - Make bubble look like a bubble with something inside
  - Fix enemy dissapearing
  - Better mouse interruptions
  - Allow touch devises
  - Allow keyboard
  - Add powerups

-}
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { bubble : Bubble
    , enemies : List Enemy
    , powerups : List Powerup
    , window : Window.Size
    , time : Time
    , pause : Bool
    , seed : Random.Seed
    }


type alias Bubble =
    { pos : Vec2
    , vel : Float
    , acc : Float
    , radius : Float
    , animationX : Animation
    , animationY : Animation
    , powerups : List PowerupType
    }


type PowerupType
    = MetalBubble


type alias Powerup =
    { pos : Vec2
    , animationX : Animation
    , animationY : Animation
    }


type EnemyType
    = Sharp
    | Bomber


type Enemy
    = SharpE SharpEnemy
    | BomberE BomberEnemy


type alias SharpEnemy =
    { pos : Vec2
    , animationX : Animation
    , animationY : Animation
    }


type alias BomberEnemy =
    { pos : Vec2
    , animationX : Animation
    , animationY : Animation
    }


init : ( Model, Cmd Msg )
init =
    ( { bubble = initBubble
      , enemies = []
      , powerups = []
      , window = initWindow
      , time = 0
      , pause = False
      , seed = Random.initialSeed 0
      }
    , Task.perform WindowResize Window.size
    )


initBubble =
    { pos = vec2 50 50
    , vel = 0
    , acc = 0
    , radius = 20
    , animationX = animation 0
    , animationY = animation 0
    , powerups = []
    }


initWindow =
    Window.Size 0 0



-- UPDATE


type Msg
    = Click Mouse.Position
    | GenerateEnemy EnemyType Time
    | GeneratePowerup PowerupType Time
    | Tick Time
    | WindowResize Window.Size
    | KeyPress Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            ( moveBubble pos model, Cmd.none )

        KeyPress key ->
            ( { model | pause = not model.pause }, Cmd.none )

        Tick delta ->
            ( model
                |> updateTime delta
                |> animateBubble
                |> moveEnemies
                |> movePowerups
                |> removeHiddenEnemies
                |> removeHiddenPowerups
            , Cmd.none
            )

        WindowResize size ->
            ( { model | window = size }, Cmd.none )

        GenerateEnemy enemyType time ->
            ( generateEnemy enemyType time model, Cmd.none )

        GeneratePowerup powerup time ->
            ( generatePowerup powerup time model, Cmd.none )


updateTime : Time -> Model -> Model
updateTime delta model =
    { model | time = model.time + delta }


moveBubble : Mouse.Position -> Model -> Model
moveBubble pos model =
    { model | bubble = moveTo pos model.time model.bubble }


animateBubble : Model -> Model
animateBubble model =
    let
        bubble =
            model.bubble

        newBubble =
            { bubble
                | pos =
                    vec2 (Animation.animate model.time bubble.animationX)
                        (Animation.animate model.time bubble.animationY)
            }
    in
    { model | bubble = newBubble }


bubbleSpeed =
    0.5


moveTo : Mouse.Position -> Time -> Bubble -> Bubble
moveTo pos time bubble =
    {-
       -- TODO: smoother interruptions
          if
              Animation.isRunning time bubble.animationX
                  || Animation.isRunning time bubble.animationY
          then
              { bubble
                  | animationX =
                      retarget time (toFloat pos.x) bubble.animationX
                  , animationY =
                      retarget time (toFloat pos.y) bubble.animationY
              }
          else
    -}
    { bubble
        | animationX =
            animation time
                |> from (getX bubble.pos)
                |> to (toFloat pos.x)
                |> speed bubbleSpeed
        , animationY =
            animation time
                |> from (getY bubble.pos)
                |> to (toFloat pos.y)
                |> speed bubbleSpeed
    }


movePowerups : Model -> Model
movePowerups model =
    { model | powerups = List.map (movePowerup model.time) model.powerups }


movePowerup : Time -> Powerup -> Powerup
movePowerup time powerup =
    { powerup
        | pos =
            vec2 (Animation.animate time powerup.animationX)
                (Animation.animate time powerup.animationY)
    }


moveEnemies : Model -> Model
moveEnemies model =
    { model | enemies = List.map (moveEnemy model.time) model.enemies }


removeHiddenPowerups : Model -> Model
removeHiddenPowerups model =
    { model | powerups = List.filter (isStillAnimating model.time) model.powerups }


removeHiddenEnemies : Model -> Model
removeHiddenEnemies model =
    { model
        | enemies =
            List.filter
                (\e ->
                    case e of
                        SharpE e ->
                            -- keep if still moving
                            isStillAnimating model.time e

                        BomberE e ->
                            -- keep if we're still within the explosion window
                            -- (end animatino time + 5 seconds)
                            (animationEndTime e + (2 * Time.second)) > model.time
                )
                model.enemies
    }


type alias Animated a =
    { a
        | animationX : Animation
        , animationY : Animation
    }


isStillAnimating : Time -> Animated a -> Bool
isStillAnimating time animated =
    not (Animation.isDone time animated.animationX)
        || not (Animation.isDone time animated.animationY)


animationEndTime : Animated a -> Time
animationEndTime animated =
    Basics.max (Animation.getStart animated.animationX + Animation.getDuration animated.animationX) (Animation.getStart animated.animationY + Animation.getDuration animated.animationY)


moveEnemy : Time -> Enemy -> Enemy
moveEnemy time enemy =
    case enemy of
        SharpE e ->
            SharpE
                { e
                    | pos =
                        vec2 (Animation.animate time e.animationX)
                            (Animation.animate time e.animationY)
                }

        BomberE e ->
            BomberE
                { e
                    | pos =
                        vec2 (Animation.animate time e.animationX)
                            (Animation.animate time e.animationY)
                }


sharpEnemySpeed =
    0.1


powerupSpeed =
    0.05


generatePointOutsideWindow : ( Float, Float ) -> Random.Generator ( Float, Float )
generatePointOutsideWindow ( width, height ) =
    Random.andThen2
        (\b1 b2 ->
            case ( b1, b2 ) of
                ( True, True ) ->
                    -- Left Side
                    Random.map (\y -> ( -50, y )) (Random.float 0 height)

                ( True, False ) ->
                    -- Right Side
                    Random.map (\y -> ( width + 50, y )) (Random.float 0 height)

                ( False, True ) ->
                    -- Top Side
                    Random.map (\x -> ( x, -50 )) (Random.float 0 width)

                ( False, False ) ->
                    -- Bottom Side
                    Random.map (\x -> ( x, height + 50 )) (Random.float 0 height)
        )
        Random.bool
        Random.bool


generatePointInsideWindow : ( Float, Float ) -> Random.Generator ( Float, Float )
generatePointInsideWindow ( width, height ) =
    Random.map2
        (,)
        (Random.float 15 (width - 15))
        (Random.float 15 (height - 15))


generatePowerup : PowerupType -> Time -> Model -> Model
generatePowerup powerupType time model =
    case powerupType of
        MetalBubble ->
            let
                pointGen =
                    generatePointOutsideWindow ( toFloat model.window.width, toFloat model.window.height )

                ( ( fromX, fromY ), seed2 ) =
                    Random.step pointGen model.seed

                ( ( toX, toY ), seed3 ) =
                    Random.step pointGen seed2

                newPowerup =
                    { pos = vec2 fromX fromY
                    , animationX = animation model.time |> from fromX |> to toX |> speed powerupSpeed
                    , animationY = animation model.time |> from fromY |> to toY |> speed powerupSpeed
                    }
            in
            { model
                | seed = seed3
                , powerups = newPowerup :: model.powerups
            }


generateEnemy : EnemyType -> Time -> Model -> Model
generateEnemy enemyType time model =
    case enemyType of
        Sharp ->
            generateSharpEnemy time model

        Bomber ->
            generateBomberEnemy time model


generateSharpEnemy : Time -> Model -> Model
generateSharpEnemy time model =
    let
        pointGen =
            generatePointOutsideWindow ( toFloat model.window.width, toFloat model.window.height )

        ( ( fromX, fromY ), seed2 ) =
            Random.step pointGen model.seed

        ( ( toX, toY ), seed3 ) =
            Random.step pointGen seed2

        newEnemy =
            { pos = vec2 fromX fromY
            , animationX = animation model.time |> from fromX |> to toX |> speed sharpEnemySpeed
            , animationY = animation model.time |> from fromY |> to toY |> speed sharpEnemySpeed
            }
    in
    { model
        | seed = seed3
        , enemies = SharpE newEnemy :: model.enemies
    }


generateBomberEnemy : Time -> Model -> Model
generateBomberEnemy time model =
    let
        outsidePointGen =
            generatePointOutsideWindow ( toFloat model.window.width, toFloat model.window.height )

        insidePointGen =
            generatePointInsideWindow ( toFloat model.window.width, toFloat model.window.height )

        ( ( fromX, fromY ), seed2 ) =
            Random.step outsidePointGen model.seed

        ( ( toX, toY ), seed3 ) =
            Random.step insidePointGen seed2

        newEnemy =
            { pos = vec2 fromX fromY
            , animationX = animation model.time |> from fromX |> to toX |> duration (5 * Time.second)
            , animationY = animation model.time |> from fromY |> to toY |> duration (5 * Time.second)
            }
    in
    { model
        | seed = seed3
        , enemies = BomberE newEnemy :: model.enemies
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width model.window.width
            , height model.window.height
            , viewBox ("0 0 " ++ toString model.window.width ++ " " ++ toString model.window.height)
            ]
            [ viewBubble model.time model.bubble
            ]
        , viewEnemies model.time model.enemies
        , viewHuman model.time model.bubble
        , viewPowerups model.time model.powerups
        ]


baseStretch =
    6


viewHuman : Time -> Bubble -> Html Msg
viewHuman time bubble =
    let
        x =
            getX bubble.pos - (bubble.radius / 2)

        y =
            getY bubble.pos - (bubble.radius / 2)
    in
    div
        [ Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "width", "20px" )
            , ( "height", "22px" )
            , ( "left", px x )
            , ( "top", px y )
            , ( "background-image", "url('daftman-sprite.jpg')" )
            , ( "background-position", "-127px -9px" )
            , ( "z-index", "-1" )
            ]
        ]
        []


viewBubble : Time -> Bubble -> Html Msg
viewBubble time bubble =
    let
        velocityX =
            abs <| Animation.velocity time bubble.animationX

        velocityY =
            abs <| Animation.velocity time bubble.animationY

        xRadius =
            bubble.radius + (baseStretch * velocityX)

        yRadius =
            bubble.radius + (baseStretch * velocityY)
    in
    g
        [ transform <| "translate(" ++ (toString <| getX bubble.pos) ++ "," ++ (toString <| getY bubble.pos) ++ ")"
        ]
        [ ellipse
            [ rx (toString xRadius)
            , ry (toString yRadius)
            , fill "lightblue"
            , stroke "blue"
            , fillOpacity "0.4"
            ]
            []
        ]


viewPowerups : Time -> List Powerup -> Html Msg
viewPowerups time powerups =
    div
        [ id "powerups"
        , Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", "0" )
            , ( "height", "100%" )
            , ( "width", "100%" )
            ]
        ]
        (List.map (viewPowerup time) powerups)


viewPowerup : Time -> Powerup -> Html Msg
viewPowerup time powerup =
    div
        [ class "metal"
        , Html.Attributes.style
            [ ( "border-radius", "50%" )
            , ( "border", "1px solid black" )
            , ( "height", "20px" )
            , ( "width", "20px" )
            , ( "background-color", "silver" )
            , ( "opacity", "0.7" )
            , ( "position", "absolute" )
            , ( "box-shadow", "0px 0px 5px 10px #8ce3ff" )
            , ( "top", px <| getY powerup.pos )
            , ( "left", px <| getX powerup.pos )
            ]
        ]
        []


viewEnemies : Time -> List Enemy -> Html Msg
viewEnemies time enemies =
    div [ id "enemies" ] (List.map (viewEnemy time) enemies)


viewEnemy : Time -> Enemy -> Html Msg
viewEnemy time enemy =
    case enemy of
        SharpE e ->
            viewSharpEnemy time e

        BomberE e ->
            viewBomberEnemy time e


viewBomberEnemy : Time -> BomberEnemy -> Html Msg
viewBomberEnemy time bomberEnemy =
    let
        boomClass =
            if isStillAnimating time bomberEnemy then
                ""
            else
                "countdown"
    in
    div
        [ class <| "bomber " ++ boomClass
        , Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "top", px <| getY bomberEnemy.pos )
            , ( "left", px <| getX bomberEnemy.pos )
            ]
        ]
        [ Html.text "💣" ]


viewSharpEnemy : Time -> SharpEnemy -> Html Msg
viewSharpEnemy time sharpEnemy =
    div
        [ Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "top", px <| getY sharpEnemy.pos )
            , ( "left", px <| getX sharpEnemy.pos )
            ]
        ]
        [ svg []
            [ path
                [ Svg.Attributes.d "M29.181 19.070c-1.679-2.908-0.669-6.634 2.255-8.328l-3.145-5.447c-0.898 0.527-1.943 0.829-3.058 0.829-3.361 0-6.085-2.742-6.085-6.125h-6.289c0.008 1.044-0.252 2.103-0.811 3.070-1.679 2.908-5.411 3.897-8.339 2.211l-3.144 5.447c0.905 0.515 1.689 1.268 2.246 2.234 1.676 2.903 0.672 6.623-2.241 8.319l3.145 5.447c0.895-0.522 1.935-0.82 3.044-0.82 3.35 0 6.067 2.725 6.084 6.092h6.289c-0.003-1.034 0.259-2.080 0.811-3.038 1.676-2.903 5.399-3.894 8.325-2.219l3.145-5.447c-0.899-0.515-1.678-1.266-2.232-2.226zM16 22.479c-3.578 0-6.479-2.901-6.479-6.479s2.901-6.479 6.479-6.479c3.578 0 6.479 2.901 6.479 6.479s-2.901 6.479-6.479 6.479z"
                , fill "red"
                , stroke "darkred"
                ]
                []
            ]
        ]


px : Float -> String
px x =
    toString x ++ "px"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.pause then
        Sub.batch
            [ Keyboard.presses KeyPress
            ]
    else
        Sub.batch
            ([ Mouse.clicks Click
             , Window.resizes WindowResize
             , AnimationFrame.diffs Tick
             , Keyboard.presses KeyPress
             ]
                ++ enemyGenerator model
                ++ powerupGenerator model
            )


enemyGenerator : Model -> List (Sub Msg)
enemyGenerator model =
    [ -- Time.every (Time.second * 2) (GenerateEnemy Sharp)
      Time.every (Time.second * 5) (GenerateEnemy Bomber)
    ]


powerupGenerator : Model -> List (Sub Msg)
powerupGenerator model =
    [ Time.every (Time.second * 5) (GeneratePowerup MetalBubble) ]
