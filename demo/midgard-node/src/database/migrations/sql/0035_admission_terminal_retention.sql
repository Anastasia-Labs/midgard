-- class: D; retention: a terminal `tx_admissions` row is deleted once it is older than `ADMISSION_TERMINAL_RETENTION_MS` and neither a pending row nor a rejection names its tx; `tx_admission_payloads` keeps a row only until its admission's terminal mark
-- Nothing reads a payload after its admission's terminal mark: acceptance
-- copies the bytes into the mempool table, peer sharing reads that copy, and
-- the duplicate check compares the full hash on the admission row (migration
-- 34). The terminal marks now delete the payload in their own transaction;
-- this removes the payloads of rows already terminal.
DELETE FROM public.tx_admission_payloads AS payload
USING public.tx_admissions AS admission
WHERE admission.tx_id = payload.tx_id
  AND admission.status = ANY (ARRAY['accepted'::public.tx_admission_status, 'rejected'::public.tx_admission_status]);

-- The retention sweep walks terminal rows oldest first.
CREATE INDEX idx_tx_admissions_terminal ON public.tx_admissions USING btree (terminal_at, tx_id) WHERE (status = ANY (ARRAY['accepted'::public.tx_admission_status, 'rejected'::public.tx_admission_status]));
