-- class: D; retention: unchanged (two narrow columns move onto `tx_admissions`; `tx_admission_payloads` loses its hash column)
-- The admission row carries its payload's size and its canonical V1 full
-- hash, so neither the backlog byte quota nor the duplicate check reads the
-- payload table. The in-memory backlog gauge reconciles its byte count with
-- SUM(payload_bytes) over the live rows; the duplicate check compares the
-- full hash, which covers the witnesses, so equal hashes mean equal bytes.
-- The hash moves: the payload table no longer stores it.
ALTER TABLE public.tx_admissions
    ADD COLUMN payload_bytes integer,
    ADD COLUMN tx_full_hash_v1 bytea;

UPDATE public.tx_admissions AS admission
SET payload_bytes = octet_length(payload.tx_canonical_cbor),
    tx_full_hash_v1 = payload.tx_full_hash_v1
FROM public.tx_admission_payloads AS payload
WHERE payload.tx_id = admission.tx_id;

ALTER TABLE public.tx_admissions
    ALTER COLUMN payload_bytes SET NOT NULL,
    ALTER COLUMN tx_full_hash_v1 SET NOT NULL,
    ADD CONSTRAINT tx_admissions_payload_bytes_check CHECK ((payload_bytes > 0)),
    ADD CONSTRAINT tx_admissions_tx_full_hash_v1_check CHECK ((octet_length(tx_full_hash_v1) = 32));

ALTER TABLE public.tx_admission_payloads
    DROP COLUMN tx_full_hash_v1;
