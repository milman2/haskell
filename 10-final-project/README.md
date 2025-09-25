# 10단계: 최종 프로젝트

## 학습 목표
- 지금까지 학습한 모든 Haskell 개념을 종합적으로 활용
- 실무 수준의 완전한 애플리케이션 개발
- 프로젝트 설계와 아키텍처 이해
- 테스트, 문서화, 배포까지 포함한 전체 개발 과정

## 프로젝트 선택 가이드

### 추천 프로젝트 옵션

#### 1. 웹 기반 할 일 관리 시스템 (Todo App)
**기술 스택**: Yesod, Persistent, SQLite, JavaScript
**주요 기능**:
- 사용자 인증 및 권한 관리
- 할 일 CRUD 작업
- 실시간 업데이트 (WebSocket)
- 파일 첨부 기능
- 검색 및 필터링

#### 2. 블로그 플랫폼
**기술 스택**: Scotty, Persistent, PostgreSQL, HTML/CSS
**주요 기능**:
- 게시글 작성 및 편집 (Markdown 지원)
- 댓글 시스템
- 태그 및 카테고리
- RSS 피드
- 관리자 대시보드

#### 3. 채팅 애플리케이션
**기술 스택**: Yesod, STM, WebSocket, SQLite
**주요 기능**:
- 실시간 메시징
- 채팅방 관리
- 파일 공유
- 사용자 상태 표시
- 메시지 히스토리

#### 4. 데이터 분석 도구
**기술 스택**: Servant, Persistent, PostgreSQL, Chart.js
**주요 기능**:
- CSV 파일 업로드 및 파싱
- 데이터 시각화
- 통계 계산
- 리포트 생성
- 데이터 내보내기

#### 5. API 게이트웨이
**기술 스택**: Servant, STM, Redis, Docker
**주요 기능**:
- 요청 라우팅
- 인증 및 권한 부여
- 레이트 리미팅
- 로깅 및 모니터링
- 캐싱

## 프로젝트 구조 예시

### 웹 기반 할 일 관리 시스템

```
todo-app/
├── app/
│   ├── Main.hs
│   └── DevelMain.hs
├── src/
│   ├── Application.hs
│   ├── Foundation.hs
│   ├── Handler/
│   │   ├── Home.hs
│   │   ├── Auth.hs
│   │   ├── Todo.hs
│   │   └── Api.hs
│   ├── Model/
│   │   ├── User.hs
│   │   ├── Todo.hs
│   │   └── File.hs
│   ├── Import.hs
│   └── Settings.hs
├── config/
│   ├── routes.yesodroutes
│   └── settings.yml
├── static/
│   ├── css/
│   ├── js/
│   └── img/
├── templates/
│   ├── default-layout.hamlet
│   ├── home.hamlet
│   └── todo.hamlet
├── test/
│   ├── Spec.hs
│   └── Handler/
├── package.yaml
├── stack.yaml
└── README.md
```

## 구현 단계별 가이드

### 1단계: 프로젝트 설정
```bash
# Yesod 프로젝트 생성
stack new todo-app yesod-sqlite
cd todo-app
stack build
stack exec yesod devel
```

### 2단계: 데이터베이스 모델 설계
```haskell
-- src/Model/User.hs
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    email Text
    password Text
    name Text
    createdAt UTCTime
    UniqueUserEmail email
    deriving Show
|]

-- src/Model/Todo.hs
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    title Text
    description Text
    completed Bool
    priority Priority
    dueDate Maybe UTCTime
    userId UserId
    createdAt UTCTime
    updatedAt UTCTime
    deriving Show

Priority
    name Text
    level Int
    deriving Show
|]
```

### 3단계: API 엔드포인트 구현
```haskell
-- src/Handler/Api.hs
module Handler.Api where

import Import
import Data.Aeson
import Data.Time

-- 할 일 목록 조회
getApiTodosR :: Handler Value
getApiTodosR = do
    userId <- requireAuthId
    todos <- runDB $ selectList [TodoUserId ==. userId] [Desc TodoCreatedAt]
    returnJson $ map todoToJson todos

-- 할 일 생성
postApiTodosR :: Handler Value
postApiTodosR = do
    userId <- requireAuthId
    todoData <- requireJsonBody :: Handler TodoData
    now <- liftIO getCurrentTime
    
    let todo = Todo
            { todoTitle = todoTitle todoData
            , todoDescription = todoDescription todoData
            , todoCompleted = False
            , todoPriority = todoPriority todoData
            , todoDueDate = todoDueDate todoData
            , todoUserId = userId
            , todoCreatedAt = now
            , todoUpdatedAt = now
            }
    
    todoId <- runDB $ insert todo
    returnJson $ object ["id" .= todoId, "message" .= "Todo created successfully"]

-- 할 일 수정
putApiTodoR :: TodoId -> Handler Value
putApiTodoR todoId = do
    userId <- requireAuthId
    todoData <- requireJsonBody :: Handler TodoUpdateData
    maybeTodo <- runDB $ get todoId
    
    case maybeTodo of
        Nothing -> notFound
        Just todo -> do
            when (todoUserId todo /= userId) $ notFound
            now <- liftIO getCurrentTime
            let updatedTodo = todo
                    { todoTitle = todoUpdateTitle todoData
                    , todoDescription = todoUpdateDescription todoData
                    , todoCompleted = todoUpdateCompleted todoData
                    , todoPriority = todoUpdatePriority todoData
                    , todoDueDate = todoUpdateDueDate todoData
                    , todoUpdatedAt = now
                    }
            runDB $ replace todoId updatedTodo
            returnJson $ object ["message" .= "Todo updated successfully"]

-- 할 일 삭제
deleteApiTodoR :: TodoId -> Handler Value
deleteApiTodoR todoId = do
    userId <- requireAuthId
    maybeTodo <- runDB $ get todoId
    
    case maybeTodo of
        Nothing -> notFound
        Just todo -> do
            when (todoUserId todo /= userId) $ notFound
            runDB $ delete todoId
            returnJson $ object ["message" .= "Todo deleted successfully"]
```

### 4단계: 프론트엔드 구현
```javascript
// static/js/todo.js
class TodoApp {
    constructor() {
        this.todos = [];
        this.init();
    }
    
    init() {
        this.loadTodos();
        this.bindEvents();
    }
    
    async loadTodos() {
        try {
            const response = await fetch('/api/todos');
            this.todos = await response.json();
            this.renderTodos();
        } catch (error) {
            console.error('Error loading todos:', error);
        }
    }
    
    async createTodo(todoData) {
        try {
            const response = await fetch('/api/todos', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify(todoData)
            });
            
            if (response.ok) {
                this.loadTodos();
            }
        } catch (error) {
            console.error('Error creating todo:', error);
        }
    }
    
    async updateTodo(id, todoData) {
        try {
            const response = await fetch(`/api/todos/${id}`, {
                method: 'PUT',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify(todoData)
            });
            
            if (response.ok) {
                this.loadTodos();
            }
        } catch (error) {
            console.error('Error updating todo:', error);
        }
    }
    
    async deleteTodo(id) {
        try {
            const response = await fetch(`/api/todos/${id}`, {
                method: 'DELETE'
            });
            
            if (response.ok) {
                this.loadTodos();
            }
        } catch (error) {
            console.error('Error deleting todo:', error);
        }
    }
    
    renderTodos() {
        const container = document.getElementById('todos-container');
        container.innerHTML = '';
        
        this.todos.forEach(todo => {
            const todoElement = this.createTodoElement(todo);
            container.appendChild(todoElement);
        });
    }
    
    createTodoElement(todo) {
        const div = document.createElement('div');
        div.className = `todo-item ${todo.completed ? 'completed' : ''}`;
        div.innerHTML = `
            <h3>${todo.title}</h3>
            <p>${todo.description}</p>
            <div class="todo-actions">
                <button onclick="todoApp.toggleTodo(${todo.id})">
                    ${todo.completed ? '완료 취소' : '완료'}
                </button>
                <button onclick="todoApp.deleteTodo(${todo.id})">삭제</button>
            </div>
        `;
        return div;
    }
    
    bindEvents() {
        document.getElementById('todo-form').addEventListener('submit', (e) => {
            e.preventDefault();
            const formData = new FormData(e.target);
            const todoData = {
                title: formData.get('title'),
                description: formData.get('description'),
                priority: formData.get('priority'),
                dueDate: formData.get('dueDate')
            };
            this.createTodo(todoData);
            e.target.reset();
        });
    }
}

// 앱 초기화
const todoApp = new TodoApp();
```

### 5단계: 테스트 작성
```haskell
-- test/Handler/TodoSpec.hs
module Handler.TodoSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Todo API" $ do
        it "creates a new todo" $ do
            user <- createUser "test@example.com" "password" "Test User"
            authenticateAs user
            
            let todoData = object
                    [ "title" .= ("Test Todo" :: Text)
                    , "description" .= ("Test Description" :: Text)
                    , "priority" .= ("high" :: Text)
                    ]
            
            request $ do
                setMethod "POST"
                setUrl ApiTodosR
                setRequestBody $ encode todoData
                addRequestHeader ("Content-Type", "application/json")
            
            statusIs 200
            jsonResponse $ \json -> do
                json @?= object ["message" .= ("Todo created successfully" :: Text)]
        
        it "retrieves todos for authenticated user" $ do
            user <- createUser "test@example.com" "password" "Test User"
            authenticateAs user
            
            -- 할 일 생성
            todo <- createTodo user "Test Todo" "Test Description"
            
            request $ do
                setMethod "GET"
                setUrl ApiTodosR
            
            statusIs 200
            jsonResponse $ \json -> do
                let todos = json ^. key "todos"
                length (todos ^. _Array) @?= 1
```

### 6단계: 배포 설정
```yaml
# docker-compose.yml
version: '3.8'
services:
  app:
    build: .
    ports:
      - "3000:3000"
    environment:
      - DATABASE_URL=postgresql://user:password@db:5432/todoapp
    depends_on:
      - db
  
  db:
    image: postgres:13
    environment:
      - POSTGRES_DB=todoapp
      - POSTGRES_USER=user
      - POSTGRES_PASSWORD=password
    volumes:
      - postgres_data:/var/lib/postgresql/data

volumes:
  postgres_data:
```

## 평가 기준

### 기능 구현 (40%)
- 요구사항 충족도
- 코드 품질
- 에러 처리

### 아키텍처 (25%)
- 모듈 구조
- 설계 패턴 활용
- 확장성

### 테스트 (20%)
- 단위 테스트
- 통합 테스트
- 테스트 커버리지

### 문서화 (10%)
- README 작성
- API 문서
- 코드 주석

### 배포 (5%)
- 배포 설정
- 환경 구성
- 운영 고려사항

## 추가 도전 과제

### 고급 기능
1. 실시간 협업 편집
2. 모바일 앱 API
3. 마이크로서비스 분리
4. 머신러닝 통합
5. 블록체인 연동

### 성능 최적화
1. 캐싱 전략
2. 데이터베이스 최적화
3. 비동기 처리
4. CDN 활용
5. 로드 밸런싱

## 완료 후 다음 단계

### 실무 준비
1. 오픈소스 기여
2. 기술 블로그 작성
3. 컨퍼런스 발표
4. 취업 포트폴리오 구성

### 심화 학습
1. 고급 타입 시스템
2. 컴파일러 개발
3. 함수형 프로그래밍 이론
4. 분산 시스템 설계

## 결론

이 10단계 학습 과정을 통해 Haskell의 기본 개념부터 실무 수준의 애플리케이션 개발까지 체계적으로 학습할 수 있습니다. 각 단계를 차근차근 진행하면서 함수형 프로그래밍의 핵심 개념을 이해하고, 실무에서 활용할 수 있는 수준까지 도달할 수 있을 것입니다.

꾸준한 연습과 프로젝트 경험을 통해 Haskell의 강력한 타입 시스템과 함수형 프로그래밍의 장점을 체감할 수 있기를 바랍니다.
