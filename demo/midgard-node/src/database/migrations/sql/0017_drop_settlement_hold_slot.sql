-- class: B; retention: unchanged (a settlement attempt's row is never deleted: it keeps the signed body and its fee reservation)
-- A settlement attempt no longer holds the event-history journal's anchor
-- back. Its L1 outcome is read from the intent journal (0015), and nothing
-- that settles an event reads the event-history block applications, so the
-- slot each attempt recorded for that hold goes.
ALTER TABLE settlement_attempts DROP COLUMN hold_slot;
