# 9단계: 웹 개발과 실무 프로젝트

## 학습 목표
- Yesod/Scotty 웹 프레임워크 사용법
- 데이터베이스 연동 (Persistent)
- REST API 설계와 구현
- 인증과 보안
- 실무 프로젝트 개발 경험

## 학습 내용

### 1. 웹 프레임워크
- Yesod: 풀스택 웹 프레임워크
- Scotty: 경량 웹 프레임워크
- Servant: 타입 안전한 API 서버

### 2. 데이터베이스 연동
- Persistent ORM
- 데이터베이스 마이그레이션
- 쿼리 최적화

### 3. REST API 설계
- RESTful 원칙
- JSON API 설계
- API 문서화

### 4. 인증과 보안
- JWT 토큰 인증
- 비밀번호 해싱
- CSRF 보호

### 5. 실무 개발
- 프로젝트 구조 설계
- 테스트 작성
- 배포와 운영

## 프로젝트: REST API와 웹 애플리케이션

### 구현할 기능
1. 사용자 관리 시스템
2. 블로그 API
3. 실시간 채팅 웹 애플리케이션
4. 파일 업로드 시스템

### 예제 코드

#### UserAPI.hs (사용자 관리 API)
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UserAPI where

import Yesod
import Yesod.Auth
import Yesod.Auth.HashDB
import Yesod.Persist
import Yesod.Persist.Core
import Database.Persist.Sqlite
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Crypto.Scrypt
import Data.Aeson
import Control.Monad.IO.Class

-- 데이터베이스 모델
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    email Text
    password Text
    name Text
    createdAt UTCTime
    UniqueUserEmail email
    deriving Show
|]

-- Yesod 애플리케이션 타입
data App = App
    { appConnPool :: ConnectionPool
    }

-- Yesod 인스턴스
instance Yesod App where
    makeSessionBackend _ = return Nothing

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

-- 인증 설정
instance YesodAuth App where
    type AuthId App = UserId
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [authHashDB (Just . UniqueUserEmail)]
    authHttpManager = error "No manager needed"

instance YesodAuthPersist App

-- 라우트 정의
mkYesod "App" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
/api/users UsersR GET POST
/api/users/#UserId UserR GET PUT DELETE
|]

-- API 핸들러
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
    <h1>사용자 관리 API
    <p>API 엔드포인트:
    <ul>
        <li>GET /api/users - 사용자 목록
        <li>POST /api/users - 사용자 생성
        <li>GET /api/users/{id} - 사용자 조회
        <li>PUT /api/users/{id} - 사용자 수정
        <li>DELETE /api/users/{id} - 사용자 삭제
|]

-- 사용자 목록 조회
getUsersR :: Handler Value
getUsersR = do
    users <- runDB $ selectList [] []
    returnJson $ map userToJson users

-- 사용자 생성
postUsersR :: Handler Value
postUsersR = do
    userData <- requireJsonBody :: Handler UserData
    now <- liftIO getCurrentTime
    hashedPassword <- liftIO $ hashPassword (userPassword userData)
    
    let user = User
            { userEmail = userEmail userData
            , userPassword = hashedPassword
            , userName = userName userData
            , userCreatedAt = now
            }
    
    userId <- runDB $ insert user
    returnJson $ object ["id" .= userId, "message" .= "User created successfully"]

-- 사용자 조회
getUserR :: UserId -> Handler Value
getUserR userId = do
    maybeUser <- runDB $ get userId
    case maybeUser of
        Nothing -> notFound
        Just user -> returnJson $ userToJson (Entity userId user)

-- 사용자 수정
putUserR :: UserId -> Handler Value
putUserR userId = do
    userData <- requireJsonBody :: Handler UserUpdateData
    maybeUser <- runDB $ get userId
    case maybeUser of
        Nothing -> notFound
        Just user -> do
            let updatedUser = user
                    { userName = userUpdateName userData
                    }
            runDB $ replace userId updatedUser
            returnJson $ object ["message" .= "User updated successfully"]

-- 사용자 삭제
deleteUserR :: UserId -> Handler Value
deleteUserR userId = do
    runDB $ delete userId
    returnJson $ object ["message" .= "User deleted successfully"]

-- 데이터 타입 정의
data UserData = UserData
    { userEmail :: Text
    , userPassword :: Text
    , userName :: Text
    } deriving (Show, Generic)

instance FromJSON UserData

data UserUpdateData = UserUpdateData
    { userUpdateName :: Text
    } deriving (Show, Generic)

instance FromJSON UserUpdateData

-- 유틸리티 함수
userToJson :: Entity User -> Value
userToJson (Entity userId user) = object
    [ "id" .= userId
    , "email" .= userEmail user
    , "name" .= userName user
    , "createdAt" .= userCreatedAt user
    ]

hashPassword :: Text -> IO Text
hashPassword password = do
    hash <- encryptPassIO (T.unpack password)
    return $ T.pack hash

-- 메인 함수
main :: IO ()
main = do
    pool <- createSqlitePool "users.db" 10
    runSqlPool (runMigration migrateAll) pool
    warp 3000 $ App pool
```

#### BlogAPI.hs (블로그 API)
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module BlogAPI where

import Yesod
import Yesod.Persist
import Database.Persist.Sqlite
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Aeson
import Control.Monad.IO.Class

-- 데이터베이스 모델
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post
    title Text
    content Text
    authorId UserId
    createdAt UTCTime
    updatedAt UTCTime
    published Bool
    deriving Show

Comment
    postId PostId
    authorName Text
    content Text
    createdAt UTCTime
    deriving Show
|]

-- Yesod 애플리케이션
data BlogApp = BlogApp
    { blogConnPool :: ConnectionPool
    }

instance Yesod BlogApp

instance YesodPersist BlogApp where
    type YesodPersistBackend BlogApp = SqlBackend
    runDB action = do
        BlogApp pool <- getYesod
        runSqlPool action pool

-- 라우트 정의
mkYesod "BlogApp" [parseRoutes|
/ HomeR GET
/api/posts PostsR GET POST
/api/posts/#PostId PostR GET PUT DELETE
/api/posts/#PostId/comments CommentsR GET POST
|]

-- 홈페이지
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
    <h1>블로그 API
    <p>API 엔드포인트:
    <ul>
        <li>GET /api/posts - 게시글 목록
        <li>POST /api/posts - 게시글 생성
        <li>GET /api/posts/{id} - 게시글 조회
        <li>PUT /api/posts/{id} - 게시글 수정
        <li>DELETE /api/posts/{id} - 게시글 삭제
        <li>GET /api/posts/{id}/comments - 댓글 목록
        <li>POST /api/posts/{id}/comments - 댓글 생성
|]

-- 게시글 목록 조회
getPostsR :: Handler Value
getPostsR = do
    posts <- runDB $ selectList [PostPublished ==. True] [Desc PostCreatedAt]
    returnJson $ map postToJson posts

-- 게시글 생성
postPostsR :: Handler Value
postPostsR = do
    postData <- requireJsonBody :: Handler PostData
    now <- liftIO getCurrentTime
    
    let post = Post
            { postTitle = postTitle postData
            , postContent = postContent postData
            , postAuthorId = postAuthorId postData
            , postCreatedAt = now
            , postUpdatedAt = now
            , postPublished = postPublished postData
            }
    
    postId <- runDB $ insert post
    returnJson $ object ["id" .= postId, "message" .= "Post created successfully"]

-- 게시글 조회
getPostR :: PostId -> Handler Value
getPostR postId = do
    maybePost <- runDB $ get postId
    case maybePost of
        Nothing -> notFound
        Just post -> do
            comments <- runDB $ selectList [CommentPostId ==. postId] [Asc CommentCreatedAt]
            returnJson $ postWithCommentsToJson (Entity postId post) comments

-- 게시글 수정
putPostR :: PostId -> Handler Value
putPostR postId = do
    postData <- requireJsonBody :: Handler PostUpdateData
    maybePost <- runDB $ get postId
    case maybePost of
        Nothing -> notFound
        Just post -> do
            now <- liftIO getCurrentTime
            let updatedPost = post
                    { postTitle = postUpdateTitle postData
                    , postContent = postUpdateContent postData
                    , postUpdatedAt = now
                    }
            runDB $ replace postId updatedPost
            returnJson $ object ["message" .= "Post updated successfully"]

-- 게시글 삭제
deletePostR :: PostId -> Handler Value
deletePostR postId = do
    runDB $ delete postId
    returnJson $ object ["message" .= "Post deleted successfully"]

-- 댓글 목록 조회
getCommentsR :: PostId -> Handler Value
getCommentsR postId = do
    comments <- runDB $ selectList [CommentPostId ==. postId] [Asc CommentCreatedAt]
    returnJson $ map commentToJson comments

-- 댓글 생성
postCommentsR :: PostId -> Handler Value
postCommentsR postId = do
    commentData <- requireJsonBody :: Handler CommentData
    now <- liftIO getCurrentTime
    
    let comment = Comment
            { commentPostId = postId
            , commentAuthorName = commentAuthorName commentData
            , commentContent = commentContent commentData
            , commentCreatedAt = now
            }
    
    commentId <- runDB $ insert comment
    returnJson $ object ["id" .= commentId, "message" .= "Comment created successfully"]

-- 데이터 타입 정의
data PostData = PostData
    { postTitle :: Text
    , postContent :: Text
    , postAuthorId :: UserId
    , postPublished :: Bool
    } deriving (Show, Generic)

instance FromJSON PostData

data PostUpdateData = PostUpdateData
    { postUpdateTitle :: Text
    , postUpdateContent :: Text
    } deriving (Show, Generic)

instance FromJSON PostUpdateData

data CommentData = CommentData
    { commentAuthorName :: Text
    , commentContent :: Text
    } deriving (Show, Generic)

instance FromJSON CommentData

-- 유틸리티 함수
postToJson :: Entity Post -> Value
postToJson (Entity postId post) = object
    [ "id" .= postId
    , "title" .= postTitle post
    , "content" .= postContent post
    , "authorId" .= postAuthorId post
    , "createdAt" .= postCreatedAt post
    , "updatedAt" .= postUpdatedAt post
    , "published" .= postPublished post
    ]

postWithCommentsToJson :: Entity Post -> [Entity Comment] -> Value
postWithCommentsToJson (Entity postId post) comments = object
    [ "id" .= postId
    , "title" .= postTitle post
    , "content" .= postContent post
    , "authorId" .= postAuthorId post
    , "createdAt" .= postCreatedAt post
    , "updatedAt" .= postUpdatedAt post
    , "published" .= postPublished post
    , "comments" .= map commentToJson comments
    ]

commentToJson :: Entity Comment -> Value
commentToJson (Entity commentId comment) = object
    [ "id" .= commentId
    , "authorName" .= commentAuthorName comment
    , "content" .= commentContent comment
    , "createdAt" .= commentCreatedAt comment
    ]

-- 메인 함수
main :: IO ()
main = do
    pool <- createSqlitePool "blog.db" 10
    runSqlPool (runMigration migrateAll) pool
    warp 3001 $ BlogApp pool
```

#### FileUpload.hs (파일 업로드 시스템)
```haskell
{-# LANGUAGE OverloadedStrings #-}

module FileUpload where

import Yesod
import Yesod.Persist
import Database.Persist.Sqlite
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Aeson
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

-- 파일 모델
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UploadedFile
    originalName Text
    fileName Text
    filePath Text
    fileSize Int
    mimeType Text
    uploadedAt UTCTime
    deriving Show
|]

-- 파일 업로드 앱
data FileApp = FileApp
    { fileConnPool :: ConnectionPool
    , uploadDir :: FilePath
    }

instance Yesod FileApp

instance YesodPersist FileApp where
    type YesodPersistBackend FileApp = SqlBackend
    runDB action = do
        FileApp pool _ <- getYesod
        runSqlPool action pool

-- 라우트 정의
mkYesod "FileApp" [parseRoutes|
/ HomeR GET
/upload UploadR GET POST
/files/#UploadedFileId FileR GET
|]

-- 홈페이지
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
    <h1>파일 업로드 시스템
    <p>
        <a href=@{UploadR}>파일 업로드
    <p>
        <a href=@{FilesR}>업로드된 파일 목록
|]

-- 파일 업로드 페이지
getUploadR :: Handler Html
getUploadR = defaultLayout [whamlet|
    <h1>파일 업로드
    <form method=post enctype=multipart/form-data>
        <p>
            <label for=file>파일 선택:
            <input type=file name=file id=file required>
        <p>
            <input type=submit value="업로드">
|]

-- 파일 업로드 처리
postUploadR :: Handler Value
postUploadR = do
    FileApp _ uploadDir <- getYesod
    (fileInfo, fileContent) <- runRequestBody
    now <- liftIO getCurrentTime
    
    let originalName = fileName fileInfo
        fileExtension = takeExtension originalName
        uniqueFileName = show (round (utctDayTime now * 1000000) :: Integer) ++ fileExtension
        filePath = uploadDir </> uniqueFileName
    
    -- 파일 저장
    liftIO $ BL.writeFile filePath fileContent
    
    -- 데이터베이스에 기록
    let uploadedFile = UploadedFile
            { uploadedFileOriginalName = originalName
            , uploadedFileName = uniqueFileName
            , uploadedFilePath = T.pack filePath
            , uploadedFileSize = fromIntegral $ BL.length fileContent
            , uploadedFileMimeType = T.pack $ fileContentType fileInfo
            , uploadedFileUploadedAt = now
            }
    
    fileId <- runDB $ insert uploadedFile
    returnJson $ object ["id" .= fileId, "message" .= "File uploaded successfully"]

-- 파일 다운로드
getFileR :: UploadedFileId -> Handler TypedContent
getFileR fileId = do
    maybeFile <- runDB $ get fileId
    case maybeFile of
        Nothing -> notFound
        Just file -> do
            let filePath = T.unpack $ uploadedFilePath file
            fileContent <- liftIO $ BL.readFile filePath
            addHeader "Content-Disposition" $ "attachment; filename=" ++ T.unpack (uploadedFileOriginalName file)
            return $ TypedContent (T.pack $ uploadedFileMimeType file) $ toContent fileContent

-- 메인 함수
main :: IO ()
main = do
    pool <- createSqlitePool "files.db" 10
    runSqlPool (runMigration migrateAll) pool
    
    -- 업로드 디렉토리 생성
    uploadDir <- getCurrentDirectory
    let uploadPath = uploadDir </> "uploads"
    createDirectoryIfMissing True uploadPath
    
    warp 3002 $ FileApp pool uploadPath
```

## 연습 문제
1. JWT 토큰을 사용한 인증 시스템을 구현하세요
2. 실시간 알림 시스템을 만들어보세요
3. 이미지 갤러리 웹 애플리케이션을 구현하세요
4. REST API 문서화 도구를 만들어보세요

## 고급 연습 문제
1. 마이크로서비스 아키텍처를 구현하세요
2. 분산 캐싱 시스템을 만들어보세요
3. 실시간 협업 도구를 구현하세요

## 테스트 방법
```bash
# 의존성 설치
stack build

# 서버 실행
stack exec user-api
stack exec blog-api
stack exec file-upload

# API 테스트
curl -X GET http://localhost:3000/api/users
curl -X POST http://localhost:3000/api/users -H "Content-Type: application/json" -d '{"email":"test@example.com","password":"password","name":"Test User"}'
```

## 다음 단계
10단계에서는 최종 프로젝트를 통해 학습한 내용을 종합적으로 활용합니다.
