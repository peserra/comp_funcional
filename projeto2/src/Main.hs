{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Main (main) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Data.Int (Int32)

import Idiom
import qualified Data.GI.Base.Overloading
import Control.Monad.IO.Class
import qualified GHC.OverloadedLabels

getCaminhoArquivo :: Gtk.Entry -> IO String
getCaminhoArquivo entry = do
    entryText <- Gtk.entryGetText entry
    let inText = T.unpack entryText
    let caminhoPasta = "./TextosInput/"
    return (caminhoPasta ++ inText ++ ".txt")


salvaArquivo :: FilePath -> String -> IO ()
salvaArquivo = writeFile 


-- cria uma janela de salvamento
abreJanelaSave :: MonadIO m => [Char] -> m ()
abreJanelaSave txtBuffer = do
    w <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.setWindowTitle w "File Name"
    Gtk.setWindowDefaultHeight w 35
    Gtk.setWindowDefaultWidth w 350
    Gtk.setWindowWindowPosition w Gtk.WindowPositionCenter

    -- Cria uma caixa horizontal para organizar o Entry e o Botão lado a lado
    hbox <- Gtk.boxNew Gtk.OrientationHorizontal 10
    Gtk.containerAdd w hbox

    -- Cria um campo de entrada (Entry)
    entry <- Gtk.entryNew
    Gtk.boxPackStart hbox entry True True 0  -- O campo de entrada preenche o espaço disponível

    -- Cria um botão ao lado do Entry
    okBtn <- Gtk.buttonNewWithLabel "Ok"

    Gtk.boxPackStart hbox okBtn False False 0  -- O botão tem tamanho fixo e fica ao lado
    #showAll w

    on okBtn #clicked $ do
        caminho <- getCaminhoArquivo entry 
        salvaArquivo caminho txtBuffer
        #destroy w
    return ()



-- chat gpt com modificações
highlightWord :: (Data.GI.Base.Overloading.IsDescendantOf Gtk.TextBuffer a,  Control.Monad.IO.Class.MonadIO m, GObject a,  GHC.OverloadedLabels.IsLabel    "getBounds" (a -> m (Gtk.TextIter, Gtk.TextIter))) => T.Text -> a -> m ()
highlightWord word buffer = do
      -- Cria uma tag para aplicar uma cor (ex: vermelho)
    tagRed <- new Gtk.TextTag [ #foreground := "red" ]
    tagTable <- Gtk.textBufferGetTagTable buffer
    Gtk.textTagTableAdd tagTable tagRed

    (startIter, endIter) <-  #getBounds buffer
    colorAllWords startIter endIter tagRed 0
   
    where
        colorAllWords sIter eIter tag offset = do
            text <- Gtk.textBufferGetText buffer sIter eIter True
            let wordPos = T.breakOn word text -- quebra o text em (..., word ...)
            if T.null (snd wordPos) -- Verifica se encontrou a palavra
                then return () -- Se não encontrou, não faz nada
                else do
                    -- calcula offset + (length do texto antes de word) para iniciar busca por word
                    let startOffset = offset + fromIntegral (T.length (fst wordPos)) :: Int32
                    let wordLength = fromIntegral (T.length word) :: Int32
                    -- cria dois ponteiros, apontando para o inicio e fim da palavra
                    wordStartIter <- Gtk.textBufferGetIterAtOffset buffer startOffset
                    wordEndIter <- Gtk.textBufferGetIterAtOffset buffer (startOffset + wordLength)
                    -- aplica a tag de cor dentro do intervalo desses ponteiros
                    Gtk.textBufferApplyTag buffer tag wordStartIter wordEndIter
                    -- seleciona o novo começo como sendo a posição seguinte da palavra no buffer
                    newStart <- Gtk.textBufferGetIterAtOffset buffer (startOffset + wordLength + 1) 
                    --calcula valor do novo offset
                    let newOffset = startOffset + wordLength + 1
                    -- chamada recursiva
                    colorAllWords newStart eIter tag newOffset 

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
    -- buffer <- Gtk.textViewGetBuffer textView
    Gtk.textViewSetWrapMode textView Gtk.WrapModeWord -- Quebra de linha automática
    Gtk.widgetSetHexpand textView True
    Gtk.widgetSetVexpand textView True

    -- os numeros sao coluna linha #linhas #colunas que ocupa
    #attach grid btnBox   0 0 1 1
    #attach grid textView 0 1 1 1

    -- Ações dos botões

    on btnNew #clicked $ do
        buffer <- #getBuffer textView
        Gtk.textBufferSetText buffer "" (-1)


    on btnSave #clicked $ do

        buffer <- #getBuffer textView
        (startIter, endIter) <- #getBounds buffer 
        content <- Gtk.textBufferGetText buffer startIter endIter True
        let bufferText = T.unpack content
        abreJanelaSave bufferText
        
        estadoInicial <- Idiom.inicializa
        erradas <- Idiom.achaListaPalavrasErradas bufferText estadoInicial
       
        let numWordsBuffer = length $ words bufferText
        if numWordsBuffer > 10 
        then mapM_ (`highlightWord` buffer) (T.pack <$> erradas) 
        else return () 
                


    Gtk.onWidgetDestroy win Gtk.mainQuit
    #showAll win
    Gtk.main

main :: IO ()
main = app
