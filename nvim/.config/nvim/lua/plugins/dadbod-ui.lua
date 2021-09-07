vim.g.db_ui_save_location = os.getenv("HOME") .. "/.cache/vim/db_ui_queries"
vim.g.dbs = { cascade = "postgres://cascade:@localhost:5432/cascade-api" }
