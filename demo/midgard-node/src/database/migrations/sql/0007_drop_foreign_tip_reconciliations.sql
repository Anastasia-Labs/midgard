-- Speculative commit mode and its foreign-tip gate are deleted (#752). Only the
-- speculative builder wrote this table, so no running node depends on its rows.
DROP TABLE foreign_tip_reconciliations;
