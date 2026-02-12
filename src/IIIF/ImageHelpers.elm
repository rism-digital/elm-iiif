module IIIF.ImageHelpers exposing (calculateImageUrlsFromInfoJson)

import IIIF.Image exposing (ImageRegion(..), ImageSize(..), ImageUri, createImageAddress, infoUriToImageUri, setImageUriRegion, setImageUriSize)
import IIIF.ImageInfo exposing (InfoJson, WidthHeight)
import IIIF.Internal.Utilities exposing (initialize)


{-| A tiled image map entry.
-}
type alias UriMap =
    { row : Int
    , col : Int
    , width : Int
    , height : Int
    , uri : String
    , pnum : Int
    }


{-| Calculates all image urls for a given scale level given an InfoJson response
-}
calculateImageUrlsFromInfoJson : Int -> Int -> InfoJson -> List UriMap
calculateImageUrlsFromInfoJson pnum scale infoJson =
    let
        defaultTList =
            { height = Just 256
            , scaleFactors = [ 1, 2, 4, 8, 16, 32 ]
            , width = 256
            }

        tiles =
            case infoJson.tiles of
                Just tileList ->
                    List.head tileList
                        |> Maybe.withDefault defaultTList

                Nothing ->
                    defaultTList

        -- iiif spec says that if there is no height, assume a square and set
        -- to the same as the width.
        tileHeight =
            Maybe.withDefault tiles.width tiles.height

        scaledWidth =
            round (toFloat infoJson.width / toFloat scale)

        scaledHeight =
            round (toFloat infoJson.height / toFloat scale)

        numRows =
            ceiling (toFloat scaledHeight / toFloat tileHeight)

        numCols =
            ceiling (toFloat scaledWidth / toFloat tiles.width)
    in
    if numRows == 1 && numCols == 1 then
        let
            newUri =
                infoUriToImageUri infoJson.id
                    |> setImageUriRegion FullRegion
                    |> setImageUriSize (WidthAndHeightSize ( scaledWidth, scaledHeight ))
                    |> createImageAddress
        in
        List.singleton (UriMap numRows numCols scaledWidth scaledHeight newUri pnum)

    else
        let
            colMapper : Int -> Int -> UriMap
            colMapper row col =
                createIiifTileUrl
                    infoJson.id
                    pnum
                    (WidthHeight infoJson.width infoJson.height)
                    ( row, col )
                    ( tiles.width, tileHeight )
                    scale

            rowMapper : Int -> List UriMap
            rowMapper row =
                initialize numCols (colMapper row)
        in
        initialize numRows rowMapper
            |> List.concat


createIiifTileUrl :
    ImageUri
    -> Int
    -> WidthHeight
    -> ( Int, Int )
    -> ( Int, Int )
    -> Int
    -> UriMap
createIiifTileUrl uri pnum { width, height } ( row, col ) ( tw, th ) s =
    let
        ulx =
            col * tw * s

        uly =
            row * th * s

        ws =
            if ulx + tw * s > width then
                round (toFloat (width - ulx + s - 1) / toFloat s)

            else
                tw

        hs =
            if uly + th * s > height then
                round (toFloat (height - uly + s - 1) / toFloat s)

            else
                th

        wid_ =
            tw * s

        wid =
            if ulx + wid_ > width then
                width - ulx

            else
                wid_

        hei_ =
            th * s

        hei =
            if uly + hei_ > height then
                height - uly

            else
                hei_

        region =
            SizeRegion { h = hei, w = wid, x = ulx, y = uly }

        size =
            WidthAndHeightSize ( ws, hs )

        newUri =
            infoUriToImageUri uri
                |> setImageUriRegion region
                |> setImageUriSize size
                |> createImageAddress
    in
    UriMap row col ws hs newUri pnum
