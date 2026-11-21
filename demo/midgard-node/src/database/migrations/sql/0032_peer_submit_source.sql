-- class: D; retention: unchanged (one wider CHECK on `tx_admissions`)
-- Another active operator may forward a transaction it accepted. Such a row
-- is admitted with source `peer`, so that a "not yet" verdict (its input is
-- not in this node's ledger yet) forgets the row instead of rejecting it for
-- good: the sender offers it again on a later L1 advance.
ALTER TABLE public.tx_admissions
    DROP CONSTRAINT tx_admissions_submit_source_check,
    ADD CONSTRAINT tx_admissions_submit_source_check CHECK ((submit_source = ANY (ARRAY['native'::text, 'backfill'::text, 'peer'::text])));
