{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- nao sei o que sao overloads, mas precisa pra funcionar melhor


module Main (main) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import GI.Gtk (dialogNew)




app :: IO ()
app = do
    Gtk.init Nothing

    win <- Gtk.windowNew Gtk.WindowTypeToplevel

    Gtk.setWindowTitle win "Haskpad++"
    Gtk.setWindowDefaultHeight win 480
    Gtk.setWindowDefaultWidth win 640
    Gtk.setWindowWindowPosition win Gtk.WindowPositionCenter

    grid <- new Gtk.Grid []
    #add win grid
    
    
    btnBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
    btnNew <- new Gtk.Button [#label := "new"]
    btnSave <- new Gtk.Button [#label := "save"]
    
    #add btnBox btnNew
    #add btnBox btnSave

    -- Cria um TextBuffer e um TextView para editar texto
    textView <- new Gtk.TextView []
    Gtk.textViewSetWrapMode textView Gtk.WrapModeWord -- Quebra de linha automática
    Gtk.widgetSetHexpand textView True
    Gtk.widgetSetVexpand textView True

    -- os numeros sao linha coluna #linhas #colunas que ocupa
    #attach grid btnBox   0 0 1 1
    #attach grid textView 0 1 1 1
    
    -- acoes dos botoes

    on btnSave #clicked $ do
        let filePath = "TextosInput/app_saved_text.txt"
        buffer <- #getBuffer textView
        (startIter, endIter) <- #getBounds buffer 
        content <- Gtk.textBufferGetText  buffer startIter endIter True
        let stringContent = T.unpack content
        -- print $ words stringContent
        writeFile filePath stringContent 



    Gtk.onWidgetDestroy win Gtk.mainQuit
    #showAll win
    Gtk.main

main :: IO ()
main = app
    
   