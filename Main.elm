module Main exposing (..)

import Animation exposing (..)
import AnimationFrame
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard
import List.Extra as List
import Math.Vector2 exposing (..)
import Maybe.Extra as Maybe
import Mouse
import Random
import Random.Extra as Random
import Task
import Time exposing (..)
import Window


{-| Next Steps

  - Add in sound effects
  - Better bubble stretch and squeeze
  - Better mouse interruptions
  - Allow touch devices
  - Allow keyboard
  - Fix bubble speedup when metal stops

-}



-- CONSTANTS


initialBubbleHealth : Int
initialBubbleHealth =
    10


bubbleRadius : Float
bubbleRadius =
    60


baseStretch : Float
baseStretch =
    -- how stretchy is the bubble?
    30


sharpVelocity : Float
sharpVelocity =
    -- speed of the sharp enemies
    0.5


bombVelocity : Float
bombVelocity =
    -- speed of the bombs
    0.3


explosionDuration : Time
explosionDuration =
    -- how long the bomb explosion lasts
    1 * Time.second



-- MAIN


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
    , isGameOver : Bool
    , score : Float
    }



-- User


type alias Bubble =
    { pos : Vec2
    , speed : Float
    , radius : Float
    , animationX : Animation
    , animationY : Animation
    , powerups : List ( StartTime, PowerupType )
    , health : Int
    , direction : Float
    , velocityX : Float
    , velocityY : Float
    }



-- Powerups


type alias StartTime =
    Time.Time


type PowerupType
    = MetalBubble


type alias Powerup =
    { pos : Vec2
    , radius : Float
    , animationX : Animation
    , animationY : Animation
    , type_ : PowerupType
    }



-- Enemies


type EnemyType
    = Sharp
    | Bomber


type Enemy
    = SharpE SharpEnemy
    | BomberE BomberEnemy


type alias SharpEnemy =
    { pos : Vec2
    , radius : Float
    , velocity : Vec2
    }


type alias BomberEnemy =
    { pos : Vec2
    , radius : Float
    , velocity : Vec2
    , countdown : Float
    }


bomber : Enemy -> Maybe BomberEnemy
bomber enemy =
    case enemy of
        SharpE _ ->
            Nothing

        BomberE b ->
            Just b


bombers : List Enemy -> List BomberEnemy
bombers enemies =
    enemies
        |> List.map bomber
        |> Maybe.values


sharpy : Enemy -> Maybe SharpEnemy
sharpy enemy =
    case enemy of
        SharpE e ->
            Just e

        BomberE _ ->
            Nothing


sharpies : List Enemy -> List SharpEnemy
sharpies enemies =
    enemies
        |> List.map sharpy
        |> Maybe.values



-- Init model


init : ( Model, Cmd Msg )
init =
    ( { bubble = initBubble 0 0
      , enemies = []
      , powerups = []
      , window = Window.Size 0 0
      , time = 0
      , pause = False
      , seed = Random.initialSeed 0
      , isGameOver = False
      , score = 0
      }
    , Task.perform InitialWindowSize Window.size
    )


initBubble : Float -> Float -> Bubble
initBubble x y =
    { pos = vec2 x y
    , speed = 0.5
    , radius = bubbleRadius
    , animationX = animation x |> from x |> to x
    , animationY = animation y |> from y |> to y
    , powerups = []
    , health = initialBubbleHealth
    , direction = 0
    , velocityX = 0
    , velocityY = 0
    }


bubbleFromWindowSize : Window.Size -> Bubble
bubbleFromWindowSize size =
    initBubble (toFloat (size.width // 2)) (toFloat (size.height // 2))



-- UPDATE


type Msg
    = StartGame
    | Tick Time
    | Click Mouse.Position
    | KeyPress Keyboard.KeyCode
    | GenerateEnemy EnemyType Time
    | GeneratePowerup PowerupType Time
    | InitialWindowSize Window.Size
    | WindowResize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            init

        Tick delta ->
            ( model
                |> updateTime delta
                |> animateBubble
                |> moveEnemies delta
                |> bombCountdown delta
                |> movePowerups
                |> removeHiddenEnemies
                |> removeHiddenPowerups
                |> checkPowerupCollisions
                |> checkEnemyCollisions
                |> timeoutPowerups
                |> checkGameOver
                |> updateScore delta
            , Cmd.none
            )

        Click pos ->
            ( moveBubble pos model, Cmd.none )

        KeyPress key ->
            if not model.isGameOver then
                -- If not game over, toggle pause
                ( { model | pause = not model.pause }, Cmd.none )
            else
                -- If game over, start over
                update StartGame model

        GenerateEnemy enemyType time ->
            ( generateEnemy enemyType time model, Cmd.none )

        GeneratePowerup powerup time ->
            ( generatePowerup powerup time model, Cmd.none )

        InitialWindowSize size ->
            ( { model | window = size, bubble = bubbleFromWindowSize size }, Cmd.none )

        WindowResize size ->
            ( { model | window = size }, Cmd.none )


updateTime : Time -> Model -> Model
updateTime delta model =
    { model | time = model.time + delta }



-- Update bubble


moveBubble : Mouse.Position -> Model -> Model
moveBubble pos model =
    { model | bubble = moveTo pos model.time model.bubble }


moveTo : Mouse.Position -> Time -> Bubble -> Bubble
moveTo pos time bubble =
    let
        bubbleSpeed =
            -- If there's a powerup, slow down the bubble
            if List.any (\( startTime, powerup ) -> powerup == MetalBubble) bubble.powerups then
                0.1
            else
                0.5
    in
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


animateBubble : Model -> Model
animateBubble model =
    let
        bubble =
            model.bubble

        ( velocityX, velocityY ) =
            ( Animation.velocity model.time bubble.animationX
            , Animation.velocity model.time bubble.animationY
            )

        direction =
            atan2 velocityY velocityX

        newBubble =
            { bubble
                | direction = direction
                , velocityX = velocityX
                , velocityY = velocityY
                , pos =
                    vec2 (Animation.animate model.time bubble.animationX)
                        (Animation.animate model.time bubble.animationY)
            }
    in
    { model | bubble = newBubble }



-- Update powerups


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


isBubbleMetal : Bubble -> Bool
isBubbleMetal bubble =
    List.any (\( startTime, powerup ) -> powerup == MetalBubble) bubble.powerups


checkPowerupCollisions : Model -> Model
checkPowerupCollisions model =
    let
        bubble =
            model.bubble

        ( collidingPowerups, nonCollidingPowerups ) =
            List.partition (areCirclesColliding bubble) model.powerups

        collidingPowerupNames =
            collidingPowerups
                |> List.map .type_
                |> List.map ((,) model.time)

        newBubble =
            { bubble
                | powerups =
                    (collidingPowerupNames ++ bubble.powerups)
                        |> List.uniqueBy toString
            }
    in
    { model | bubble = newBubble, powerups = nonCollidingPowerups }


timeoutPowerups : Model -> Model
timeoutPowerups model =
    let
        bubble =
            model.bubble

        isPowerupValid ( startTime, powerup ) =
            -- Powerups are valid for 5 seconds
            startTime + (5 * Time.second) > model.time

        newBubble =
            { bubble | powerups = List.filter isPowerupValid bubble.powerups }
    in
    { model | bubble = newBubble }


removeHiddenPowerups : Model -> Model
removeHiddenPowerups model =
    { model | powerups = List.filter (not << isOffScreen model.window) model.powerups }



-- Update Enemies / Collision detection


moveEnemies : Time -> Model -> Model
moveEnemies delta model =
    { model | enemies = List.map (moveEnemy delta) model.enemies }


moveEnemy : Time -> Enemy -> Enemy
moveEnemy time enemy =
    case enemy of
        SharpE e ->
            SharpE
                { e | pos = move time e }

        BomberE e ->
            BomberE
                { e | pos = move time e }


removeHiddenEnemies : Model -> Model
removeHiddenEnemies model =
    { model
        | enemies =
            List.filter
                (\e ->
                    case e of
                        SharpE e ->
                            -- keep if on screen
                            not (isOffScreen model.window e)

                        BomberE e ->
                            -- keep if explosion not finished
                            e.countdown > -explosionDuration
                )
                model.enemies
    }


checkEnemyCollisions : Model -> Model
checkEnemyCollisions model =
    let
        bubble =
            model.bubble

        ( collidingEnemies, nonCollidingEnemies ) =
            List.partition
                (\e ->
                    case e of
                        SharpE e ->
                            areCirclesColliding bubble e

                        BomberE e ->
                            areCirclesColliding bubble e
                                && e.countdown
                                < explosionDuration
                )
                model.enemies

        collidedBombs =
            List.map bomber nonCollidingEnemies
                |> Maybe.values

        nonCollidingNonBombs =
            List.map sharpy nonCollidingEnemies
                |> Maybe.values
                |> List.map SharpE

        newBombs =
            collidedBombs
                |> List.map (\b -> collideCircles ( bubble.pos, bubble.radius ) ( b.pos, b.radius ))
                |> List.zip collidedBombs
                |> List.map (\( bomb, res ) -> resolveCollision res bubble bomb)
                |> List.map BomberE

        newBubble =
            { bubble
                | health =
                    if isBubbleMetal bubble then
                        bubble.health
                    else
                        bubble.health - List.length collidingEnemies
            }
    in
    { model | bubble = newBubble, enemies = newBombs ++ nonCollidingNonBombs }



-- Update Bomb Enemy


bombCountdown : Time -> Model -> Model
bombCountdown delta model =
    { model
        | enemies =
            model.enemies
                |> bombers
                |> List.map (countdown delta)
                |> List.map BomberE
                |> List.append (List.map SharpE (sharpies model.enemies))
    }


countdown : Time -> BomberEnemy -> BomberEnemy
countdown delta bomber =
    { bomber | countdown = bomber.countdown - delta }



-- Game state updates


checkGameOver : Model -> Model
checkGameOver model =
    if model.bubble.health <= 0 then
        { model | isGameOver = True, pause = True }
    else
        model


updateScore : Time -> Model -> Model
updateScore delta model =
    { model | score = model.score + delta }



-- GENERATORS


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
                    , radius = 20
                    , animationX = animation model.time |> from fromX |> to toX |> speed powerupSpeed
                    , animationY = animation model.time |> from fromY |> to toY |> speed powerupSpeed
                    , type_ = MetalBubble
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

        ( toVec, seed3 ) =
            ( model.bubble.pos, seed2 )

        velocity =
            direction toVec (vec2 fromX fromY)
                |> scale sharpVelocity

        newEnemy =
            { pos = vec2 fromX fromY
            , radius = 20
            , velocity = velocity
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

        ( ( fromX, fromY ), seed2 ) =
            Random.step outsidePointGen model.seed

        ( toVec, seed3 ) =
            ( model.bubble.pos, seed2 )

        velocity =
            direction toVec (vec2 fromX fromY)
                |> scale bombVelocity

        newEnemy =
            { pos = vec2 fromX fromY
            , radius = 20
            , velocity = velocity
            , countdown = 2 * Time.second
            }
    in
    { model
        | seed = seed3
        , enemies = BomberE newEnemy :: model.enemies
    }



-- VIEW


view : Model -> Html Msg
view model =
    if model.isGameOver then
        viewGameOverScreen model
    else
        div []
            [ viewHealthbar model.bubble
            , viewScore model.score
            , viewPlayer model.time model.bubble
            , viewEnemies model.time model.enemies
            , viewPowerups model.time model.powerups
            ]


viewGameOverScreen : Model -> Html Msg
viewGameOverScreen model =
    div []
        [ div [ class "gameover" ]
            [ h1 [] [ text <| "Score: " ++ formatNumber model.score ]
            , h1 [] [ text "Press Any Key to Try Again" ]
            ]
        ]


viewHealthbar : Bubble -> Html Msg
viewHealthbar bubble =
    let
        healthPercent =
            ((toFloat bubble.health / toFloat initialBubbleHealth) * 100)
                |> toString
    in
    div [ class "healthbar", Html.Attributes.style [ ( "background", "linear-gradient(270deg, red, red " ++ healthPercent ++ "%, white " ++ healthPercent ++ "%)" ) ] ] []


viewScore : Float -> Html Msg
viewScore score =
    h2 [ class "score" ] [ text <| formatNumber score ]


viewPlayer : Time -> Bubble -> Html Msg
viewPlayer time bubble =
    div
        [ class "player"
        , Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "top", px <| getY bubble.pos )
            , ( "left", px <| getX bubble.pos )
            ]
        ]
        [ viewBubble time bubble
        , viewHuman time bubble
        ]


viewHuman : Time -> Bubble -> Html Msg
viewHuman time bubble =
    div [ class "human" ] []


viewBubble : Time -> Bubble -> Html Msg
viewBubble time bubble =
    let
        stretch =
            baseStretch * (abs bubble.velocityY + abs bubble.velocityX / 2)

        xRadius =
            bubble.radius + stretch

        yRadius =
            bubble.radius

        bubbleClass =
            if List.any (\( startTime, powerup ) -> powerup == MetalBubble) bubble.powerups then
                "metalBubble"
            else
                "bubble"
    in
    div
        [ class bubbleClass
        , Html.Attributes.style
            [ ( "height", px yRadius )
            , ( "width", px xRadius )
            , ( "transform", "rotate(" ++ toString bubble.direction ++ "rad)" )
            ]
        ]
        []


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
            , ( "height", px powerup.radius )
            , ( "width", px powerup.radius )
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
            if bomberEnemy.countdown <= explosionDuration then
                "explode"
            else
                ""
    in
    div
        [ class <| "bomber " ++ boomClass
        , Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "top", px <| getY bomberEnemy.pos )
            , ( "left", px <| getX bomberEnemy.pos )
            , ( "font-size", "2em" )
            ]
        ]
        [ Html.text "💣" ]


viewSharpEnemy : Time -> SharpEnemy -> Html Msg
viewSharpEnemy time sharpEnemy =
    let
        directionVector =
            sharpEnemy.velocity

        vectorAngle =
            atan2 (Math.Vector2.getY directionVector) (Math.Vector2.getX directionVector)

        correction =
            -- we add a correction to the angle since the emoji sword is already angled
            pi + 0.6

        direction =
            vectorAngle + correction
    in
    div
        [ Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "top", px <| getY sharpEnemy.pos )
            , ( "left", px <| getX sharpEnemy.pos )
            , ( "font-size", "2em" )
            , ( "transform", "rotate(" ++ toString direction ++ "rad)" )
            ]
        ]
        [ Html.text "🗡️" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    --- only listen to keypresses if the game is paused or over
    if model.pause || model.isGameOver then
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
    [ Time.every (Time.second * 0.5) (GenerateEnemy Sharp)
    , Time.every (Time.second * 3) (GenerateEnemy Bomber)
    ]


powerupGenerator : Model -> List (Sub Msg)
powerupGenerator model =
    [ Time.every (Time.second * 5) (GeneratePowerup MetalBubble) ]



-- HELPERS


px : Float -> String
px x =
    toString x ++ "px"


formatNumber : Float -> String
formatNumber n =
    -- TODO: handle decimal points
    formatInt (round n)


formatInt : Int -> String
formatInt n =
    toString n
        |> String.reverse
        |> String.toList
        |> List.greedyGroupsOf 3
        |> List.map String.fromList
        |> String.join ","
        |> String.reverse


isOffScreen : Window.Size -> { a | pos : Vec2 } -> Bool
isOffScreen window object =
    let
        ( x, y ) =
            ( getX object.pos, getY object.pos )
    in
    x < -60 || x > toFloat window.width || y < -60 || y > toFloat window.height



-- Collision detection (and resolution for bombs)


type alias Position =
    Vec2


type alias Radius =
    Float


type alias CollisionResult =
    { normal : Vec2, penetration : Float }


type alias CollidableCircle a =
    { a | radius : Float, pos : Vec2 }


areCirclesColliding : CollidableCircle a -> CollidableCircle b -> Bool
areCirclesColliding c1 c2 =
    let
        dist =
            Math.Vector2.sub c1.pos c2.pos
                |> Math.Vector2.lengthSquared
                |> sqrt

        rad =
            c1.radius + c2.radius
    in
    dist < rad


{-| move takes a time delta and an object with a position and velocity
and returns the new position
-}
move : Time -> { a | pos : Vec2, velocity : Vec2 } -> Vec2
move delta object =
    -- p1 = p2 + v*t
    add object.pos (scale delta object.velocity)


sharpEnemySpeed =
    0.1


powerupSpeed =
    0.05


{-| Calculate CollisionResult for two circles
-- takes position vector and radius for each circle
-}
collideCircles : ( Position, Radius ) -> ( Position, Radius ) -> CollisionResult
collideCircles ( pos0, radius0 ) ( pos1, radius1 ) =
    let
        b0b1 =
            Math.Vector2.sub pos1 pos0

        radiusb0b1 =
            radius0 + radius1

        distanceSq =
            -- simple optimization: doesn't compute sqrt unless necessary
            Math.Vector2.lengthSquared b0b1
    in
    if distanceSq == 0 then
        -- same position, arbitrary normal
        CollisionResult (vec2 1 0) radius0
    else if distanceSq >= radiusb0b1 * radiusb0b1 then
        -- no intersection, arbitrary normal
        CollisionResult (vec2 1 0) 0
    else
        let
            d =
                sqrt distanceSq
        in
        CollisionResult (Math.Vector2.scale (1 / d) b0b1) (radiusb0b1 - d)


{-| modify bodies' trajectories based off the colision result
-}
resolveCollision : CollisionResult -> Bubble -> BomberEnemy -> BomberEnemy
resolveCollision { normal, penetration } bubble bomber =
    let
        bubbleVelocity =
            vec2 bubble.velocityX bubble.velocityY

        ( bomberX, bomberY ) =
            ( getX bomber.pos, getY bomber.pos )

        relativeVelocity =
            Math.Vector2.sub bomber.velocity bubbleVelocity

        velocityAlongNormal =
            Math.Vector2.dot relativeVelocity normal

        ( bubbleInverseMass, bomberInverseMass ) =
            ( 0.3, 0.3 )

        ( bubbleRestitution, bomberRestitution ) =
            ( 0.3, 0.3 )
    in
    if penetration == 0 || velocityAlongNormal > 0 then
        bomber
        -- no collision or velocities separating
    else
        let
            restitution =
                -- collision restitution
                Basics.min bubbleRestitution bomberRestitution

            invMassSum =
                bubbleInverseMass + bomberInverseMass

            j =
                -- impulse scalar
                (-(1 + restitution) * velocityAlongNormal) / invMassSum

            impulse =
                -- impulse vector
                Math.Vector2.scale j normal

            newBomberVelocity =
                Math.Vector2.add bomber.velocity (Math.Vector2.scale bomberInverseMass impulse)

            ( bomberToX, bomberToY ) =
                ( 1000, 1000 )
        in
        { bomber | velocity = newBomberVelocity }
