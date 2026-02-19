-- Initialize SQLite3 database for hackage-server
--
-- sqlite3 hackage.db < init_db.sql
--
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY,
    username TEXT NOT NULL UNIQUE, -- CHECK: should it be unique? what happens with deleted users?
    status TEXT NOT NULL,
    auth_info TEXT,
    admin BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS user_tokens (
    id INTEGER PRIMARY KEY,
    user_id INTEGER NOT NULL,
    description TEXT NOT NULL,
    token TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS account_details (
    user_id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    contact_email TEXT NOT NULL,
    kind TEXT,
    admin_notes TEXT NOT NULL
);
