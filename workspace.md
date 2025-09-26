# VS Code & Cursor 워크스페이스 관리

## 워크스페이스 관리 방법

### 1. **워크스페이스 파일 생성**

#### **수동 생성**
```bash
# 프로젝트 루트에서
touch my-project.code-workspace
```

#### **VS Code/Cursor에서 생성**
1. `File` → `Save Workspace As...`
2. 원하는 이름으로 저장 (예: `haskell.code-workspace`)

### 2. **워크스페이스 파일 구조**

```json
{
    "folders": [
        {
            "name": "프로젝트 이름",
            "path": "./경로"
        }
    ],
    "settings": {
        "설정키": "설정값"
    },
    "extensions": {
        "recommendations": ["확장ID1", "확장ID2"]
    },
    "launch": {
        "version": "0.2.0",
        "configurations": []
    },
    "tasks": {
        "version": "2.0.0",
        "tasks": []
    }
}
```

### 3. **워크스페이스 열기**

#### **명령줄에서**
```bash
# VS Code
code my-project.code-workspace

# Cursor
cursor my-project.code-workspace
```

#### **GUI에서**
1. `File` → `Open Workspace from File...`
2. 워크스페이스 파일 선택

### 4. **워크스페이스 설정 관리**

#### **프로젝트별 설정**
```json
{
    "folders": [
        {
            "name": "Haskell Projects",
            "path": "."
        }
    ],
    "settings": {
        "haskell.serverExecutablePath": "haskell-language-server-wrapper",
        "haskell.formattingProvider": "ormolu",
        "editor.formatOnSave": true,
        "files.exclude": {
            "**/node_modules": true,
            "**/dist": true,
            "**/.stack-work": true
        }
    }
}
```

#### **언어별 설정**
```json
{
    "settings": {
        "[haskell]": {
            "editor.tabSize": 2,
            "editor.insertSpaces": true
        },
        "[javascript]": {
            "editor.tabSize": 4,
            "editor.insertSpaces": false
        }
    }
}
```

### 5. **확장 프로그램 관리**

#### **워크스페이스별 확장 프로그램**
```json
{
    "extensions": {
        "recommendations": [
            "haskell.haskell",
            "justusadam.language-haskell",
            "ms-vscode.vscode-json"
        ],
        "unwantedRecommendations": [
            "ms-vscode.vscode-typescript"
        ]
    }
}
```

#### **확장 프로그램 설치**
1. 워크스페이스 열기
2. `Ctrl+Shift+X` (확장 프로그램 패널)
3. 권장 확장 프로그램 자동 표시
4. `Install All` 클릭

### 6. **폴더 관리**

#### **여러 프로젝트 포함**
```json
{
    "folders": [
        {
            "name": "Frontend",
            "path": "./frontend"
        },
        {
            "name": "Backend",
            "path": "./backend"
        },
        {
            "name": "Shared",
            "path": "./shared"
        }
    ]
}
```

#### **폴더 추가/제거**
1. **추가**: `File` → `Add Folder to Workspace`
2. **제거**: 폴더 우클릭 → `Remove Folder from Workspace`

### 7. **태스크와 런치 설정**

#### **빌드 태스크**
```json
{
    "tasks": {
        "version": "2.0.0",
        "tasks": [
            {
                "label": "Haskell Build",
                "type": "shell",
                "command": "stack",
                "args": ["build"],
                "group": "build",
                "presentation": {
                    "echo": true,
                    "reveal": "always",
                    "focus": false,
                    "panel": "shared"
                }
            }
        ]
    }
}
```

#### **디버그 설정**
```json
{
    "launch": {
        "version": "0.2.0",
        "configurations": [
            {
                "name": "Haskell Debug",
                "type": "haskell",
                "request": "launch",
                "program": "${workspaceFolder}/app/Main.hs"
            }
        ]
    }
}
```

### 8. **워크스페이스 관리 팁**

#### **환경별 워크스페이스**
```bash
# 개발용
dev.code-workspace

# 프로덕션용
prod.code-workspace

# 테스트용
test.code-workspace
```

#### **팀 공유**
```bash
# .gitignore에 추가하지 말고 버전 관리에 포함
git add *.code-workspace

# README에 워크스페이스 사용법 문서화
echo "# 워크스페이스 설정" >> README.md
echo "1. VS Code/Cursor에서 프로젝트.code-workspace 파일 열기" >> README.md
echo "2. 권장 확장 프로그램 설치" >> README.md
```

### 9. **워크스페이스 상태 관리**

#### **워크스페이스 상태 저장**
- VS Code/Cursor는 자동으로 워크스페이스 상태를 저장
- 열린 파일, 폴더 구조, 확장 프로그램 상태 등

#### **상태 복원**
- 워크스페이스를 다시 열면 이전 상태로 복원
- `File` → `Reopen Last Workspace`

### 10. **워크스페이스 문제 해결**

#### **일반적인 문제들**
```bash
# 워크스페이스 파일이 손상된 경우
rm .vscode/workspace.json
# 또는 워크스페이스 파일을 다시 생성

# 확장 프로그램이 제대로 로드되지 않는 경우
# 워크스페이스를 닫고 다시 열기
```

#### **설정 충돌 해결**
```json
{
    "settings": {
        // 워크스페이스 설정이 사용자 설정보다 우선
        "editor.fontSize": 14,
        "workbench.colorTheme": "Dark+"
    }
}
```

### 11. **워크스페이스 최적화**

#### **성능 최적화**
```json
{
    "settings": {
        "files.watcherExclude": {
            "**/node_modules/**": true,
            "**/.git/**": true,
            "**/dist/**": true,
            "**/.stack-work/**": true
        },
        "search.exclude": {
            "**/node_modules": true,
            "**/dist": true,
            "**/.stack-work": true
        }
    }
}
```

#### **메모리 사용량 최적화**
```json
{
    "settings": {
        "typescript.preferences.includePackageJsonAutoImports": "off",
        "haskell.maxCompletions": 20,
        "editor.suggest.maxVisibleSuggestions": 10
    }
}
```

## 실제 워크스페이스 예시

### 1. **Haskell 프로젝트 워크스페이스**

```json
{
    "folders": [
        {
            "name": "Haskell Projects",
            "path": "."
        },
        {
            "name": "Enum Generator",
            "path": "./DSL/enum-generator"
        }
    ],
    "settings": {
        "haskell.serverExecutablePath": "haskell-language-server-wrapper",
        "haskell.formattingProvider": "ormolu",
        "haskell.hlint.enable": true,
        "haskell.manageHLS": "GHCup",
        "haskell.trace.server": "messages",
        "haskell.serverArgs": ["--lsp"],
        "haskell.maxCompletions": 40,
        "haskell.completionSnippetsOn": true,
        "haskell.ghcide.enable": false,
        "files.exclude": {
            "**/.stack-work": true,
            "**/dist": true,
            "**/.cabal": true
        }
    },
    "extensions": {
        "recommendations": [
            "haskell.haskell",
            "justusadam.language-haskell",
            "vigoo.stylish-haskell",
            "alanz.vscode-hie-server",
            "ms-vscode.vscode-json"
        ]
    }
}
```

### 2. **풀스택 웹 애플리케이션 워크스페이스**

```json
{
    "folders": [
        {
            "name": "Frontend (React)",
            "path": "./frontend"
        },
        {
            "name": "Backend (Node.js)",
            "path": "./backend"
        },
        {
            "name": "Database",
            "path": "./database"
        },
        {
            "name": "Documentation",
            "path": "./docs"
        }
    ],
    "settings": {
        "typescript.preferences.importModuleSpecifier": "relative",
        "editor.formatOnSave": true,
        "files.exclude": {
            "**/node_modules": true,
            "**/dist": true,
            "**/.next": true
        },
        "search.exclude": {
            "**/node_modules": true,
            "**/dist": true,
            "**/.next": true
        }
    },
    "extensions": {
        "recommendations": [
            "ms-vscode.vscode-typescript-next",
            "bradlc.vscode-tailwindcss",
            "ms-vscode.vscode-json",
            "esbenp.prettier-vscode"
        ]
    }
}
```

### 3. **마이크로서비스 아키텍처 워크스페이스**

```json
{
    "folders": [
        {
            "name": "API Gateway",
            "path": "./services/gateway"
        },
        {
            "name": "User Service",
            "path": "./services/user"
        },
        {
            "name": "Order Service",
            "path": "./services/order"
        },
        {
            "name": "Payment Service",
            "path": "./services/payment"
        },
        {
            "name": "Shared Libraries",
            "path": "./shared"
        }
    ],
    "settings": {
        "editor.formatOnSave": true,
        "files.exclude": {
            "**/node_modules": true,
            "**/dist": true,
            "**/build": true
        }
    },
    "extensions": {
        "recommendations": [
            "ms-vscode.vscode-typescript-next",
            "ms-vscode.vscode-json",
            "redhat.vscode-yaml"
        ]
    }
}
```

## 워크스페이스 설정 우선순위

1. **워크스페이스 설정** (최우선)
2. **폴더별 설정** (`.vscode/settings.json`)
3. **사용자 설정** (전역 설정)
4. **기본 설정**

## 워크스페이스 vs 단일 폴더

| 기능 | 단일 폴더 | 워크스페이스 |
|------|-----------|--------------|
| 폴더 수 | 1개 | 여러 개 |
| 설정 범위 | 전역 | 워크스페이스별 |
| 확장 프로그램 | 수동 설치 | 자동 권장 |
| 팀 협업 | 어려움 | 쉬움 |
| 복잡한 프로젝트 | 부적합 | 적합 |
| 성능 | 빠름 | 약간 느림 |

## 워크스페이스 파일 관리

### 1. **버전 관리**
```bash
# 워크스페이스 파일을 저장소에 포함
git add *.code-workspace
git commit -m "Add workspace configuration"

# .gitignore에 추가하지 않음
# echo "*.code-workspace" >> .gitignore  # 하지 말 것!
```

### 2. **환경별 분리**
```bash
# 개발 환경
dev.code-workspace

# 프로덕션 환경
prod.code-workspace

# 테스트 환경
test.code-workspace
```

### 3. **팀 공유**
```bash
# README에 워크스페이스 사용법 추가
cat >> README.md << EOF

## 워크스페이스 설정

1. VS Code 또는 Cursor에서 \`project.code-workspace\` 파일을 열어주세요.
2. 권장 확장 프로그램을 설치해주세요.
3. 프로젝트별 설정이 자동으로 적용됩니다.

EOF
```

## 요약

워크스페이스 관리는:

1. **파일 생성**: `.code-workspace` 파일 생성
2. **폴더 구성**: 여러 프로젝트를 하나의 워크스페이스에 포함
3. **설정 관리**: 프로젝트별 설정과 확장 프로그램 관리
4. **팀 협업**: 워크스페이스 파일을 버전 관리에 포함
5. **환경 분리**: 개발/프로덕션/테스트 환경별 워크스페이스
6. **성능 최적화**: 불필요한 파일 감시 제외
7. **태스크 관리**: 빌드, 디버그 설정 포함
8. **상태 관리**: 워크스페이스 상태 자동 저장/복원

이렇게 하면 복잡한 프로젝트를 체계적으로 관리할 수 있습니다!

## 참고 자료

- [VS Code 워크스페이스 공식 문서](https://code.visualstudio.com/docs/editor/workspaces)
- [VS Code 설정 공식 문서](https://code.visualstudio.com/docs/getstarted/settings)
- [VS Code 태스크 공식 문서](https://code.visualstudio.com/docs/editor/tasks)
- [VS Code 디버그 공식 문서](https://code.visualstudio.com/docs/editor/debugging)
