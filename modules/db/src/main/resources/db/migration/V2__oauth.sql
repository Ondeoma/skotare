-- ---------------------------------------------------------------- OAuth の提供側（skakapo などが利用者の権限で skotare を使う）

-- 認可で発行したトークンも api_token に置く（REST API と MCP の認証をそのまま使うため）.
-- oauth_client: 発行したクライアント（許可し直したら、前のトークンを失効させる）
-- refresh_hash: refresh token の SHA-256（使うたびに新しいトークンの組に替え、前の組は失効させる）
-- granted_at:   利用者が許可した日時（refresh で替えても引き継ぐ. 設定の画面に出す）
alter table api_token add column oauth_client text, add column refresh_hash bytea unique, add column granted_at timestamptz;
create index api_token_oauth_idx on api_token (user_id, oauth_client) where oauth_client is not null and revoked_at is null;

-- 認可コード: 60秒で失効、一度だけ使える. ハッシュだけを保存する
create table oauth_code (
  code_hash      bytea primary key,
  client_id      text not null,
  redirect_uri   text not null,
  user_id        uuid not null references app_user(id) on delete cascade,
  code_challenge text not null,                  -- PKCE（S256）
  scope          text not null check (scope in ('read', 'write')),
  expires_at     timestamptz not null,
  used_at        timestamptz
);
create index oauth_code_expires_idx on oauth_code (expires_at);
