module GLView where

-- OpenGL
import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Linear.V3
import Linear.V4
import Linear.Matrix

initGL :: IO (ShaderProgram, GL.VertexArrayObject, GL.VertexArrayObject)
initGL = do
  -- texture Texture2D $= Enabled
  shadeModel        $= Smooth
  clearColor        $= Color4 0 0 0 0.0
  clearDepth        $= 1.0
  depthFunc         $= Just Less

  --
  lineSmooth        $= Enabled
  lineWidth         $= 10.0
  blend             $= Enabled
  blendFunc         $= (SrcAlpha, OneMinusSrcAlpha)
  --blendFunc         $= (SrcAlpha, OneMinusConstantAlpha)
  --alphaFunc         $= Just (Greater, 0.2)
  --lightModelAmbient $= Color4 0.1 0.1 0.1 0.2
  --lighting        $= Enabled
  --colorMaterial     $= Just (FrontAndBack, AmbientAndDiffuse)
  colorMaterial     $= Just (GL.Front, AmbientAndDiffuse)

  -- init shaders
  spR <- simpleShaderProgramWith "test/case2/shadowRWithTex.vert"
                                 "test/case2/shadowRWithTex.frag"
           $ \ p -> do
    attribLocation p "VertexPosition" $= AttribLocation 0
    attribLocation p "VertexNormal" $= AttribLocation 1
    attribLocation p "VertexCoord" $= AttribLocation 2
    bindFragDataLocation p "FragColor" $= 0

  -- load objects
  qvaoR <- makeVAO $ do
    print "make cube Vao"
    _ <- makeBuffer ArrayBuffer cubeVert
    enableAttrib spR "VertexPosition"
    setAttrib spR "VertexPosition" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

    _ <- makeBuffer ArrayBuffer cubeNormal
    enableAttrib spR "VertexNormal"
    setAttrib spR "VertexNormal" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

    _ <- makeBuffer ArrayBuffer cubeCoord
    enableAttrib spR "VertexCoord"
    setAttrib spR "VertexCoord" ToFloat (VertexArrayDescriptor 2 Float 0 offset0)

    return ()

  pvaoR <- makeVAO $ do
    print "make plate Vao"
    _ <- makeBuffer ArrayBuffer plateVert
    enableAttrib spR "VertexPosition"
    setAttrib spR "VertexPosition" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

    _ <- makeBuffer ArrayBuffer plateNorm
    enableAttrib spR "VertexNormal"
    setAttrib spR "VertexNormal" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

    _ <- makeBuffer ArrayBuffer plateCoord
    enableAttrib spR "VertexCoord"
    setAttrib spR "VertexCoord" ToFloat (VertexArrayDescriptor 2 Float 0 offset0)

  printErrorMsg "VAO R Init"
  return (spR,pvaoR,qvaoR)

render :: (ShaderProgram, GL.VertexArrayObject, GL.VertexArrayObject)
       -> TextureObject -> M44 GLfloat -> (GLfloat,GLfloat,GLfloat,GLfloat)
       -> IO ()
render (shprgR,pvaoR,qvaoR) tex proj (qx,qy,qz,_) = do
  GL.clientState GL.VertexArray $= GL.Enabled
  GL.currentProgram GL.$= Just (program shprgR)
  let pMat = proj -- projectionMatrix (deg2rad 60) 1.0 0.1 (40::GLfloat)
      vMatX = camMatrix $ tilt (180*qx) fpsCamera
      vMatY = camMatrix $ pan (180*qy) fpsCamera
      vMatZ = camMatrix $ roll (-180*qz) fpsCamera
      vMatIni = camMatrix $ dolly (V3 0 3 (10::GLfloat))
                            fpsCamera
      mMat = V4 (V4 1.0 0.0 0.0 0.0)
                (V4 0.0 1.0 0.0 0.0)
                (V4 0.0 0.0 1.0 0.0)
                (V4 0.0 0.0 0.0 1.0)
      --mvpMat = pMat !*! (vMatX !+! vMatY) !*! vMatIni !*! mMat
      --mvpMat = pMat !*! vMatIni !*! (vMatX !+! vMatY) !*! mMat
      mvpMat = pMat !*! vMatIni !*! (vMatX !+! vMatY !+! vMatZ) !*! mMat

  setUniform shprgR "mvpMatrix" mvpMat
  setUniform shprgR "TexMap" (TextureUnit 0)

  cullFace $= Just Back  --Just Front Just FrontAndBack -- 

  withTexturesAt Texture2D [(tex,0)] $ do
    withVAO qvaoR $ 
      drawArrays Quads 0 $ fromIntegral $ length cubeVert

    withVAO pvaoR $ 
      drawArrays Quads 0 $ fromIntegral $ length plateVert
  
  GL.currentProgram GL.$= Nothing

  printErrorMsg "Draw Frame"

cubeVert, {-qColor,-} cubeNormal, cubeCoord :: [GL.GLfloat]
cubeNormal = concat
  -- Top
  [ yp, yp, yp, yp  
  -- Bottom
  , yn, yn, yn, yn
  -- Front
  , zn, zn, zn, zn 
  -- Back
  , zp, zp, zp, zp 
  -- Right
  , xp, xp, xp, xp 
  -- Left
  , xn, xn, xn, xn  
  ]
  where
    xp = [ 1.0,  0.0,  0.0]
    xn = [-1.0,  0.0,  0.0]
    yp = [ 0.0,  1.0,  0.0]
    yn = [ 0.0, -1.0,  0.0]
    zp = [ 0.0,  0.0,  1.0]
    zn = [ 0.0,  0.0, -1.0]
cubeVert = concatMap (\ (x,y,z) -> [x,y,z])
  -- Top
  [ p7,p6,p5,p4
  -- Bottom
  , p0,p1,p2,p3 
  -- Front
  , p6,p7,p1,p0
  -- Back
  , p4,p5,p3,p2
  -- Right
  , p7,p4,p2,p1
  -- Left
  , p5,p6,p0,p3
  ]
  where
    (p0:p1:p2:p3:p4:p5:p6:p7:_) = blockNodeVertex
cubeCoord = concat $ replicate 6 c
  where
    c = [ 0.0,  0.0, 1.0,  0.0, 1.0,  1.0, 0.0,  1.0]

type VrtxPos3D = (GLfloat,GLfloat,GLfloat)
blockNodeVertex :: [VrtxPos3D]
blockNodeVertex = 
  [ ( -0.5, -0.5, -0.5) -- P0
  , (  0.5, -0.5, -0.5) -- P1
  , (  0.5, -0.5,  0.5) -- P2
  , ( -0.5, -0.5,  0.5) -- P3
  , (  0.5,  0.5,  0.5) -- P4
  , ( -0.5,  0.5,  0.5) -- P5
  , ( -0.5,  0.5, -0.5) -- P6
  , (  0.5,  0.5, -0.5) -- P7
  , (  0.5,  0.0,  0.5) -- P8
  , ( -0.5,  0.0,  0.5) -- P9
  , ( -0.5,  0.0, -0.5) -- P10
  , (  0.5,  0.0, -0.5) -- P11
  ]

plateVert, plateNorm,plateCoord :: [GL.GLfloat]
plateVert =
  [ -5.5, -1.5,  5.5
  ,  5.5, -1.5,  5.5
  ,  5.5, -1.5, -5.5
  , -5.5, -1.5, -5.5
  ]
plateNorm =
  [  0.0,  1.0,  0.0
  ,  0.0,  1.0,  0.0
  ,  0.0,  1.0,  0.0
  ,  0.0,  1.0,  0.0
  ]
plateCoord =
  [  0.0,  0.0
  ,  1.0,  0.0
  ,  1.0,  1.0
  ,  0.0,  1.0
  ]

loadTextureObj :: FilePath -> IO TextureObject
loadTextureObj fn = do
  t <- readTexture fn
  case t of
    Right t' -> do
      textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
      texture2DWrap $= (Repeated, ClampToEdge)
      return t'
    Left e -> error e
