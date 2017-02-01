{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Paths_rendertext
import Foreign (Storable, Ptr, alloca, peek, allocaArray, pokeArray, sizeOf, poke)
import Foreign.Ptr (nullPtr)
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types (CFloat(..))
import Control.Monad (join, when)
import Control.Arrow ((***))
import Control.Monad (void, unless)
import Graphics.GL.Core33
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified SDL as SDL

-- Parameters
(width, height) = (800, 600)
title = "Hello world"

-- | rendertext test app
main :: IO ()
main = void $ do
  SDL.initializeAll

  -- Window parameters
  let windowDesc = SDL.defaultWindow { SDL.windowOpenGL = Just SDL.defaultOpenGL
                                     , SDL.windowInitialSize = mkV2 (width, height)
                                     }

  -- Create window and opengl context
  window <- SDL.createWindow title windowDesc
  context <- SDL.glCreateContext window

  -- Load shaders
  program <- loadShaderProgram "data/basic.vert" "data/basic.frag"

  -- Load data
  -- A triangle vertex buffer
  vbo <- uploadBuffer undefined CFloat [-1, -1, 0, 1, -1, 0, 0, 1, 0]

  -- Main loop
  let loop = do
          -- Handle events
          events <- SDL.pollEvents

          -- Find out if the user wants to quit
          let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

          -- Render
          glClearColor 0 0 0 1
          glClear GL_COLOR_BUFFER_BIT

          glEnableVertexAttribArray 0
          glBindBuffer GL_ARRAY_BUFFER vbo
          glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
          glDrawArrays GL_TRIANGLES 0 3
          glDisableVertexAttribArray 0

          SDL.glSwapWindow window

          -- Loop if we're not quitting
          unless quit loop
  loop

  -- Cleanup
  SDL.glDeleteContext context
  SDL.destroyWindow window
  SDL.quit

-- | Map a function across a pair
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

-- | Turn any pair of integers into a Num a => SDL.V2 a
mkV2 :: (Integral a, Num b) => (a, a) -> SDL.V2 b
mkV2 = uncurry SDL.V2 . mapTuple fromIntegral

-- | Abstracts a common pattern with pointers
withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

-- | Upload a list of floats into a buffer object
-- Call like this: uploadBuffer undefined CFloat [1..10]
-- The undefined is annoying and it pisses me off, sorry...
-- It can be any value of the type b though if you want.
-- Remember to delete it
uploadBuffer :: Storable b => b -> (a -> b) -> [a] -> IO GLuint
uploadBuffer v f hlist = do
  let len = length hlist
  let cfsize = sizeOf v
  let buflen = fromIntegral $ len * cfsize
  buffer <- withNewPtr (glGenBuffers 1)
  glBindBuffer GL_ARRAY_BUFFER buffer
  ptr <- allocaArray len
                     (\ptr -> pokeArray ptr (map f hlist)
                           >> glBufferData GL_ARRAY_BUFFER buflen ptr GL_STATIC_DRAW
                     )
  return buffer

-- | Load a shader and compile it, returning the id
-- A shader id is returned whether it succeeds or not so remember to delete it
loadShader :: GLenum -> T.Text -> IO GLuint
loadShader shaderType datafile = do
  -- Load source
  path <- getDataFileName (T.unpack datafile)
  source <- T.readFile path >>= return . T.unpack

  -- Compile shader
  shader <- glCreateShader shaderType
  withCString source
    (\str -> alloca
               (\ptr -> poke ptr str
                     >> glShaderSource shader 1 ptr nullPtr
               )
    )
  glCompileShader shader

  -- Check result
  isCompiled <- withNewPtr (glGetShaderiv shader GL_COMPILE_STATUS)

  -- Output any errors
  when (isCompiled == 0) $ do
    length <- withNewPtr (glGetShaderiv shader GL_INFO_LOG_LENGTH)
    error <- allocaArray (fromIntegral length)
                         (\ptr -> glGetShaderInfoLog shader length nullPtr ptr
                               >> (peekCString ptr))
    putStr $ "Failed to compile shader" ++ path ++ ":\n" ++ error

  return shader

-- | Link program
-- A program id is returned regardless of whether link succeeds, so free it
linkProgram :: GLuint -> GLuint -> IO GLuint
linkProgram vert frag = do
  -- Link program
  program <- glCreateProgram
  glAttachShader program vert
  glAttachShader program frag
  glLinkProgram program

  -- Check result
  isLinked <- withNewPtr (glGetProgramiv program GL_LINK_STATUS)

  -- Output any errors
  when (isLinked == 0) $ do
    length <- withNewPtr (glGetProgramiv program GL_INFO_LOG_LENGTH)
    error <- allocaArray (fromIntegral length)
                         (\ptr -> glGetProgramInfoLog program length nullPtr ptr
                               >> (peekCString ptr))
    putStr $ "Failed to link program:\n" ++ error

  return program

-- | Loads a vertex and fragment shader, compiles them and links them
-- A program id is returned regardless of whether this succeeds or not so free it
loadShaderProgram :: T.Text -> T.Text -> IO GLuint
loadShaderProgram v f = do
  vert <- loadShader GL_VERTEX_SHADER v
  frag <- loadShader GL_FRAGMENT_SHADER f
  prog <- linkProgram vert frag
  glDeleteShader vert
  glDeleteShader frag
  return prog
