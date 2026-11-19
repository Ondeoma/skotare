-- ---------------------------------------------------------------- stui との連携（OAuth のクライアント側）

-- 利用者ごとの stui のトークン（APP_SECRET の鍵で暗号化. 通知先「stui のチャンネル」は、作った人のトークンで投稿する）
create table integration_token (
  user_id    uuid not null references app_user(id) on delete cascade,
  provider   text not null,
  token_enc  text not null,
  created_at timestamptz not null default now(),
  primary key (user_id, provider)
);

-- 認可を始めたときの state と PKCE の code_verifier（10分で失効）
create table integration_state (
  state         text primary key,
  user_id       uuid not null references app_user(id) on delete cascade,
  provider      text not null,
  code_verifier text not null,
  return_to     text not null,
  expires_at    timestamptz not null
);
