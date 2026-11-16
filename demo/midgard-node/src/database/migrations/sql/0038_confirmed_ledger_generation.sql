-- class: D-x; retention: unchanged (one column on `node_confirmed_ledger_frontier`, kept with its one row; the sequence it draws from is never reset)
-- The confirmed-ledger generation: which write to `confirmed_ledger` and
-- the frontier a reader saw. A verification of `confirmed_ledger` records
-- the generation it read with the frontier, and a commit builds from that
-- verification only while the frontier it reads carries the same one; so a
-- row changed outside a fold, or a frontier unfolded and folded back to the
-- same header and root, is never taken for the ledger that was verified.
--
-- The sequence is owned by no table, so `TRUNCATE ... RESTART IDENTITY`
-- never resets it and a generation is never drawn twice. Every frontier
-- write draws one (its insert by the column's default, its update by the
-- writer), and every statement that writes `confirmed_ledger`, whatever
-- writes it, draws one onto the frontier row through the trigger, in the
-- writing transaction. Without a frontier row the trigger updates nothing.
CREATE SEQUENCE node_confirmed_ledger_generation;

ALTER TABLE node_confirmed_ledger_frontier
  ADD COLUMN generation bigint NOT NULL
    DEFAULT nextval('node_confirmed_ledger_generation');

CREATE FUNCTION bump_confirmed_ledger_generation() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  UPDATE node_confirmed_ledger_frontier
  SET generation = nextval('node_confirmed_ledger_generation');
  RETURN NULL;
END;
$$;

CREATE TRIGGER confirmed_ledger_generation
  AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON confirmed_ledger
  FOR EACH STATEMENT EXECUTE FUNCTION bump_confirmed_ledger_generation();
