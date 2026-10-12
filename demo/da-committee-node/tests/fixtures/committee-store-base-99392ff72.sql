-- A committee store the BASE build (99392ff72, the C1 merge) wrote: pg_dump
-- --inserts --no-owner --no-privileges of a store that build opened, its
-- retirement fixture compacted once and seeded one header (source state,
-- signature, published and pending outbox effects, capacity evidence,
-- retirement floor), plus two outbox rows a pre-C1 build wrote under the
-- removed external-provider source mode. pg_dump's \restrict lines are
-- left out. Restored by tests/store-l1-records-upgrade.test.ts.

--
-- PostgreSQL database dump
--


-- Dumped from database version 17.6
-- Dumped by pg_dump version 17.6

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: committee_store_count_rows(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.committee_store_count_rows() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  delta bigint;
BEGIN
  IF TG_OP = 'INSERT' THEN
    SELECT count(*) INTO delta FROM new_rows;
  ELSE
    SELECT -count(*) INTO delta FROM old_rows;
  END IF;
  IF delta <> 0 THEN
    UPDATE committee_store_counts SET value = value + delta
     WHERE name = TG_ARGV[0];
  END IF;
  RETURN NULL;
END
$$;


--
-- Name: committee_store_count_submitted_headers(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.committee_store_count_submitted_headers() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  hashes text[] := '{}';
  undos integer[] := '{}';
  delta bigint;
BEGIN
  IF TG_OP <> 'INSERT' THEN
    SELECT hashes || coalesce(array_agg(header_hash), '{}'),
           undos || coalesce(array_agg((record->>'resultStatus' IN ('submitted', 'confirmed'))::integer), '{}')
      INTO hashes, undos FROM old_rows;
  END IF;
  IF TG_OP <> 'DELETE' THEN
    SELECT hashes || coalesce(array_agg(header_hash), '{}'),
           undos || coalesce(array_agg(-(record->>'resultStatus' IN ('submitted', 'confirmed'))::integer), '{}')
      INTO hashes, undos FROM new_rows;
  END IF;
  SELECT coalesce(sum((now_rows > 0)::integer - (now_rows + undo > 0)::integer), 0)
    INTO delta
    FROM (
      SELECT c.header_hash, sum(c.undo) AS undo,
             (SELECT count(*) FROM committee_l1_submissions s
               WHERE s.header_hash = c.header_hash
                 AND s.record->>'resultStatus' IN ('submitted', 'confirmed')) AS now_rows
        FROM unnest(hashes, undos) AS c(header_hash, undo)
       GROUP BY c.header_hash
    ) headers;
  IF delta <> 0 THEN
    UPDATE committee_store_counts SET value = value + delta
     WHERE name = 'submitted_or_confirmed_headers';
  END IF;
  RETURN NULL;
END
$$;


--
-- Name: committee_store_count_verified_payloads(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.committee_store_count_verified_payloads() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  delta bigint := 0;
  changed bigint;
BEGIN
  IF TG_OP <> 'INSERT' THEN
    SELECT count(*) INTO changed FROM old_rows
     WHERE record->>'validationStatus' = 'verified';
    delta := delta - changed;
  END IF;
  IF TG_OP <> 'DELETE' THEN
    SELECT count(*) INTO changed FROM new_rows
     WHERE record->>'validationStatus' = 'verified';
    delta := delta + changed;
  END IF;
  IF delta <> 0 THEN
    UPDATE committee_store_counts SET value = value + delta
     WHERE name = 'verified_payloads';
  END IF;
  RETURN NULL;
END
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: committee_da_attestation_candidates; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_da_attestation_candidates (
    header_hash text NOT NULL,
    out_ref text NOT NULL,
    record jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: committee_da_conflict_evidence; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_da_conflict_evidence (
    deployment_fingerprint text NOT NULL,
    evidence_hash text NOT NULL,
    header_hash text NOT NULL,
    commitment_digest text NOT NULL,
    conflicting_commitment_digest text NOT NULL,
    signer_index integer NOT NULL,
    reporter_peer_id text NOT NULL,
    record jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT committee_da_conflict_eviden_conflicting_commitment_diges_check CHECK ((conflicting_commitment_digest ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT committee_da_conflict_evidence_check CHECK ((conflicting_commitment_digest > commitment_digest)),
    CONSTRAINT committee_da_conflict_evidence_commitment_digest_check CHECK ((commitment_digest ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT committee_da_conflict_evidence_deployment_fingerprint_check CHECK ((deployment_fingerprint ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT committee_da_conflict_evidence_evidence_hash_check CHECK ((evidence_hash ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT committee_da_conflict_evidence_header_hash_check CHECK ((header_hash ~ '^[0-9a-f]{56}$'::text)),
    CONSTRAINT committee_da_conflict_evidence_reporter_peer_id_check CHECK ((length(reporter_peer_id) > 0)),
    CONSTRAINT committee_da_conflict_evidence_signer_index_check CHECK (((signer_index >= 0) AND (signer_index <= 255)))
);


--
-- Name: committee_da_payloads; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_da_payloads (
    header_hash text NOT NULL,
    record jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: committee_da_signatures; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_da_signatures (
    header_hash text NOT NULL,
    commitment_digest text NOT NULL,
    signer_index integer NOT NULL,
    record jsonb NOT NULL,
    end_time_ms bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT committee_da_signatures_commitment_digest_check CHECK ((commitment_digest ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT committee_da_signatures_end_time_ms_check CHECK ((end_time_ms >= 0)),
    CONSTRAINT committee_da_signatures_signer_index_check CHECK (((signer_index >= 0) AND (signer_index <= 255)))
);


--
-- Name: committee_decision_outbox; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_decision_outbox (
    effect_id text NOT NULL,
    header_hash text NOT NULL,
    record jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: committee_deployment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_deployment (
    id integer NOT NULL,
    marker_schema_version text NOT NULL,
    manifest_id text NOT NULL,
    manifest_sha256 text NOT NULL,
    contract_deployment_info_sha256 text NOT NULL,
    manifest_raw text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT committee_deployment_contract_deployment_info_sha256_check CHECK ((contract_deployment_info_sha256 ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT committee_deployment_id_check CHECK ((id = 1)),
    CONSTRAINT committee_deployment_manifest_id_check CHECK ((manifest_id ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT committee_deployment_manifest_sha256_check CHECK ((manifest_sha256 ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT committee_deployment_marker_schema_version_check CHECK ((marker_schema_version = 'midgard-deployment-marker-v1'::text))
);


--
-- Name: committee_l1_source_state; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_l1_source_state (
    id integer NOT NULL,
    record jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT committee_l1_source_state_id_check CHECK ((id = 1))
);


--
-- Name: committee_l1_submissions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_l1_submissions (
    header_hash text NOT NULL,
    tx_kind text NOT NULL,
    tx_hash text NOT NULL,
    record jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: committee_peer_broadcasts; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_peer_broadcasts (
    peer_id text NOT NULL,
    header_hash text NOT NULL,
    commitment_digest text NOT NULL,
    signer_index integer NOT NULL,
    record jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT committee_peer_broadcasts_commitment_digest_check CHECK ((commitment_digest ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT committee_peer_broadcasts_signer_index_check CHECK (((signer_index >= 0) AND (signer_index <= 255)))
);


--
-- Name: committee_peer_health; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_peer_health (
    peer_id text NOT NULL,
    record jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: committee_peer_nonces; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_peer_nonces (
    deployment_fingerprint text NOT NULL,
    signer_index integer NOT NULL,
    nonce text NOT NULL,
    record jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT committee_peer_nonces_signer_index_check CHECK (((signer_index >= 0) AND (signer_index <= 255)))
);


--
-- Name: committee_promise_capacity_evidence; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_promise_capacity_evidence (
    evidence_key text NOT NULL,
    record jsonb NOT NULL,
    CONSTRAINT committee_promise_capacity_evidence_evidence_key_check CHECK ((evidence_key ~ '^[0-9a-f]{64}$'::text))
);


--
-- Name: committee_retirement_metadata; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_retirement_metadata (
    id integer NOT NULL,
    record jsonb NOT NULL,
    CONSTRAINT committee_retirement_metadata_id_check CHECK ((id = 1))
);


--
-- Name: committee_state_queue_headers; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_state_queue_headers (
    header_hash text NOT NULL,
    record jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: committee_store_counts; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.committee_store_counts (
    name text NOT NULL,
    value bigint NOT NULL,
    CONSTRAINT committee_store_counts_value_check CHECK ((value >= 0))
);


--
-- Data for Name: committee_da_attestation_candidates; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_da_attestation_candidates VALUES ('73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0', '7777777777777777777777777777777777777777777777777777777777777777#0', '{"bitmap": "01", "outRef": "7777777777777777777777777777777777777777777777777777777777777777#0", "status": "burned", "datumCbor": "80", "threshold": 1, "headerHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "attestationCount": 1, "observedChainPoint": {"slot": 100, "depth": 5000, "blockHash": "1212121212121212121212121212121212121212121212121212121212121212", "finalized": true, "blockHeight": 100, "providerSource": "authenticated_state_queue_transition_v1"}, "committeeSignersHash": "7f26c5a0c2219f58abcbc2ebd2da349acb10773ffbc37b6af91fa8df2486c9ea", "deploymentFingerprint": "abababababababababababababababababababababababababababababababab"}', '2026-10-07 23:49:23.317569-05', '2026-10-07 23:49:23.317569-05');


--
-- Data for Name: committee_da_conflict_evidence; Type: TABLE DATA; Schema: public; Owner: -
--



--
-- Data for Name: committee_da_payloads; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_da_payloads VALUES ('73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0', '{"fetchedAt": "2026-10-02T00:00:00.000Z", "headerHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "rootSummary": {"utxosRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8", "depositsRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8", "eventToStepRoot": "6ccb9c11a4bec000df61ad38bbf071a5687d57d3254ff4b172ca8aa686a4ac11", "withdrawalsRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8", "transactionsRoot": "a713c7c168f3b744260b1c19513d968d95f284f13db85681f3a4a3386b50f476", "transitionTraceRoot": "2b11d07988496b21835ee3acb3f13086355f2202f907951f63047b4604595393", "validationTracesRoot": "c5647c7428e49410fa02e263cbffcebff4abd15b597a33bc812e2274bc68d0ed", "forcedTransactionsRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"}, "sourcePeerId": "peer1", "payloadSha256": "05a03ac54f804e4917622674ecaef3c18fc67d5bc9f88009afb60f0ad91676d9", "payloadCborHex": "8501001909495820ae632648484754449d4de347f27e60a9578be7d314d1d690a851077e866519f5590949d8799f01d8799f581c73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0d8799f58200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a85820a713c7c168f3b744260b1c19513d968d95f284f13db85681f3a4a3386b50f47658200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858202b11d07988496b21835ee3acb3f13086355f2202f907951f63047b460459539358206ccb9c11a4bec000df61ad38bbf071a5687d57d3254ff4b172ca8aa686a4ac115820c5647c7428e49410fa02e263cbffcebff4abd15b597a33bc812e2274bc68d0ed00000100010101000100000000581c00000000000000000000000000000000000000000000000000000000581c2222222222222222222222222222222222222222222222222222222201ff8080809f9f582075c4ea27a41fe204572bdf9c2bcc0b21b76ca74eeca5337a5f726065e27d334a5f5840d8799f582075c4ea27a41fe204572bdf9c2bcc0b21b76ca74eeca5337a5f726065e27d334ad8799f5f584084018c582045b0cfc220ceec5b7c1c62c4d4193d385840e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce758405f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c584062c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0002020582045b0cfc220ceec5b7c1c62c4d4193d38e4eb5840a48e8815729ce75f9c0ab0e4c1c05820455840b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d41958403d38e4eba48e8815729ce75f9c0ab05840e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a583b45c2e1e87088bfbdfa29577ae15840b62aba60e095e3ab5318ff582052408b440567f475c3eee21c9a8be91ed05a31e81e175c69fcaec5ccd187258c00ff5f584083582045b0cfc220ceec5b7c1c625840c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c58270ab0e4c1c0582045b0cfc2205829ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0ff4a89010101010101010101ffffffffff9f9f582075c4ea27a41fe204572bdf9c2bcc0b21b76ca74eeca5337a5f726065e27d334a5f584084018c418041804180002020418041804180582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a582045c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff8341804180418000ffffff8080809f9f41005f5840d8799f0100d87b9f582075c4ea27a41fe204572bdf9c2bcc0b21b76ca74eeca5337a5f726065e27d334affd87b8058200e5751c026e543b2e8ab2eb06099daa15833d1e5df47778f7787faab45cdf12fe3a858200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8ffffffff9f9f5826d87b9f582075c4ea27a41fe204572bdf9c2bcc0b21b76ca74eeca5337a5f726065e27d334aff48d8799f00d87b80ffffff9f9f5826d87b9f582075c4ea27a41fe204572bdf9c2bcc0b21b76ca74eeca5337a5f726065e27d334aff5f5840d8799f0101582069d7e8dd9534896c7cd0d5019c86ccb57b590a7d37ce3e699cae8ab8214dfc00015820259dd348d5024362b43af7f0f669d63ec55c96ba096058403172668971b1efb104b058205693a1ea0b9359fb1b5831659f24744a22774833792ceabedd6e3fcfb3c6b57ad87a805820000000000000000000000000000000520000000000000000000000000000000000ffffffff9f9f582bd8799fd87b9f582075c4ea27a41fe204572bdf9c2bcc0b21b76ca74eeca5337a5f726065e27d334aff22ff5f5840d8799fd8799f015820ebef9a2d6d41a443ec5cfa47029c3f0a83261ecd3e93b96d99403a151c7c94f3582075c4ea27a41fe204572bdf9c2bcc0b21b76ca74eec5840a5337a5f726065e27d334a58200000000000000000000000000000000000000000000000000000000000000000582018596e15a3e7ba5b1a9426d88f3d045e535840f5c8c15ed746cd2fd72372e4f10305d8798058200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8d87980005820af4a97d9bc7858406ed691a6c789f75abd8ae561d5ebd353cd612634fcea56a259b30000d8798058200000000000000000000000000000000000000000000000000000000000000058400058200000000000000000000000000000000000000000000000000000000000000000ffd8799f005820259dd348d5024362b43af7f0f669d63ec55c96ba096058373172668971b1efb104b09f5820c49791b81cbf2fdf8b3339293ad79b6964703f9a20519b20a17b71deea6c7de7ffff20004180d87980ffffff9f582bd8799fd87b9f582075c4ea27a41fe204572bdf9c2bcc0b21b76ca74eeca5337a5f726065e27d334aff23ff5f5840d8799fd8799f015820ebef9a2d6d41a443ec5cfa47029c3f0a83261ecd3e93b96d99403a151c7c94f3582075c4ea27a41fe204572bdf9c2bcc0b21b76ca74eec5840a5337a5f726065e27d334a58200000000000000000000000000000000000000000000000000000000000000000582018596e15a3e7ba5b1a9426d88f3d045e535840f5c8c15ed746cd2fd72372e4f10305d8798058200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8d9050780005820c6aff8431958403001c93f2bae287434de85d23136c463a6d01432fc12a881408c8b0000d87a8058200000000000000000000000000000000000000000000000000000000000005840000058200000000000000000000000000000000000000000000000000000000000000000ffd8799f0158205693a1ea0b9359fb1b5831659f24744a227748337958382ceabedd6e3fcfb3c6b57a9f58200ee83bc9b4912c307ae7dbe077401c08620e08f3f62eefe8e05bfee0774566b7ffff0e004180d87980ffffffffd8799f00000100010101ffffff", "validationStatus": "verified", "payloadFetchStatus": "available", "payloadSchemaVersion": 1, "deploymentFingerprint": "abababababababababababababababababababababababababababababababab"}', '2026-10-07 23:49:23.257577-05', '2026-10-07 23:49:23.257577-05');


--
-- Data for Name: committee_da_signatures; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_da_signatures VALUES ('73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0', 'c81c0e2d0483b9417a88ec445d6662261a78ca4c7b22e0252671f0b7400d8c1f', 0, '{"source": "local", "signedAt": "2026-10-02T00:00:00.000Z", "headerHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "validation": {"l1Header": {"endTime": "1", "startTime": "0", "operatorVkey": "22222222222222222222222222222222222222222222222222222222", "prevHeaderHash": "00000000000000000000000000000000000000000000000000000000", "protocolVersion": "1"}, "headerHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "rootsMatch": true, "rootSummary": {"utxosRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8", "depositsRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8", "eventToStepRoot": "6ccb9c11a4bec000df61ad38bbf071a5687d57d3254ff4b172ca8aa686a4ac11", "withdrawalsRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8", "transactionsRoot": "a713c7c168f3b744260b1c19513d968d95f284f13db85681f3a4a3386b50f476", "transitionTraceRoot": "2b11d07988496b21835ee3acb3f13086355f2202f907951f63047b4604595393", "validationTracesRoot": "c5647c7428e49410fa02e263cbffcebff4abd15b597a33bc812e2274bc68d0ed", "forcedTransactionsRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"}, "countSummary": {"depositCount": {"value": "0", "__midgardWatcherType": "bigint"}, "totalEventCount": {"value": "1", "__midgardWatcherType": "bigint"}, "withdrawalCount": {"value": "0", "__midgardWatcherType": "bigint"}, "l2TransactionCount": {"value": "1", "__midgardWatcherType": "bigint"}, "transitionStepCount": {"value": "1", "__midgardWatcherType": "bigint"}, "validationTraceCount": {"value": "1", "__midgardWatcherType": "bigint"}, "forcedTransactionCount": {"value": "0", "__midgardWatcherType": "bigint"}}, "payloadVersion": 1, "stateQueueOutRef": "6666666666666666666666666666666666666666666666666666666666666666#1"}, "payloadHash": "05a03ac54f804e4917622674ecaef3c18fc67d5bc9f88009afb60f0ad91676d9", "signerIndex": 0, "l1ChainPoint": {"slot": 100, "depth": 5000, "blockHash": "1212121212121212121212121212121212121212121212121212121212121212", "finalized": true, "blockHeight": 100, "providerSource": "authenticated_state_queue_transition_v1"}, "broadcastStatus": "posted", "signatureWitness": "007c7d24862b4e4d836cfccd86c17192ca9d42913986366eb437a7610662d2983f05f6ea6e2e5bcb74ce5d8aa1afae49b9be9f65da5b557dc3d70264b5a06c5f04", "committeeSignersHash": "7f26c5a0c2219f58abcbc2ebd2da349acb10773ffbc37b6af91fa8df2486c9ea", "deploymentFingerprint": "abababababababababababababababababababababababababababababababab", "availabilityCommitmentCbor": "d8799f01581c11111111111111111111111111111111111111111111111111111111581c73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0190974d8799f1936c41a0040000010ff9fd8799f0000190974015820b1fc7b68826dc0e0e79f31b50fd6a7a9bfae89099ae2be96b15e5deb7fc38cd3582045806652008255baf4ac4da765c043e0e62aa6dc3dc9ec7ea338a524e4bd00f4ffffff", "availabilityCommitmentDigest": "c81c0e2d0483b9417a88ec445d6662261a78ca4c7b22e0252671f0b7400d8c1f"}', 1, '2026-10-07 23:49:23.275849-05', '2026-10-07 23:49:23.291455-05');


--
-- Data for Name: committee_decision_outbox; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_decision_outbox VALUES ('abababababababababababababababababababababababababababababababab:73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0:6666666666666666666666666666666666666666666666666666666666666666#1:signature_publish:0', '73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0', '{"slot": 100, "status": "published", "network": "Preprod", "effectId": "abababababababababababababababababababababababababababababababab:73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0:6666666666666666666666666666666666666666666666666666666666666666#1:signature_publish:0", "blockHash": "1212121212121212121212121212121212121212121212121212121212121212", "createdAt": "2026-10-02T00:00:00.000Z", "finalized": true, "updatedAt": "2026-10-02T00:00:00.000Z", "effectKind": "signature_publish", "headerHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "sourceMode": "local_node", "signerIndex": 0, "attemptCount": 1, "schemaVersion": 1, "stateQueueOutRef": "6666666666666666666666666666666666666666666666666666666666666666#1", "deploymentFingerprint": "abababababababababababababababababababababababababababababababab"}', '2026-10-07 23:49:23.275849-05', '2026-10-07 23:49:23.291455-05');
INSERT INTO public.committee_decision_outbox VALUES ('abababababababababababababababababababababababababababababababab:73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0:6666666666666666666666666666666666666666666666666666666666666666#1:l1_reconcile:-', '73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0', '{"slot": 100, "status": "pending", "network": "Preprod", "effectId": "abababababababababababababababababababababababababababababababab:73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0:6666666666666666666666666666666666666666666666666666666666666666#1:l1_reconcile:-", "blockHash": "1212121212121212121212121212121212121212121212121212121212121212", "createdAt": "2026-10-02T00:00:00.000Z", "finalized": true, "updatedAt": "2026-10-02T00:00:00.000Z", "effectKind": "l1_reconcile", "headerHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "sourceMode": "local_node", "attemptCount": 1, "schemaVersion": 1, "stateQueueOutRef": "6666666666666666666666666666666666666666666666666666666666666666#1", "deploymentFingerprint": "abababababababababababababababababababababababababababababababab"}', '2026-10-07 23:49:23.354488-05', '2026-10-07 23:49:23.354488-05');
INSERT INTO public.committee_decision_outbox VALUES ('abababababababababababababababababababababababababababababababab:a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1:a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3#0:l1_reconcile:-', 'a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1', '{"slot": 100, "status": "pending", "network": "Preprod", "effectId": "abababababababababababababababababababababababababababababababab:a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1:a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3#0:l1_reconcile:-", "blockHash": "1212121212121212121212121212121212121212121212121212121212121212", "createdAt": "2026-10-01T00:00:00.000Z", "finalized": true, "updatedAt": "2026-10-01T00:00:00.000Z", "effectKind": "l1_reconcile", "headerHash": "a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1", "sourceMode": "external_providers", "attemptCount": 1, "schemaVersion": 1, "stateQueueOutRef": "a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3#0", "deploymentFingerprint": "abababababababababababababababababababababababababababababababab"}', '2026-10-07 23:49:23.387437-05', '2026-10-07 23:49:23.387437-05');
INSERT INTO public.committee_decision_outbox VALUES ('abababababababababababababababababababababababababababababababab:a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2:a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3#0:l1_reconcile:-', 'a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2', '{"slot": 100, "status": "failed", "network": "Preprod", "effectId": "abababababababababababababababababababababababababababababababab:a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2:a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3#0:l1_reconcile:-", "blockHash": "1212121212121212121212121212121212121212121212121212121212121212", "createdAt": "2026-10-01T00:00:00.000Z", "finalized": true, "lastError": "L1 source quarantined", "updatedAt": "2026-10-01T00:00:00.000Z", "effectKind": "l1_reconcile", "headerHash": "a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2", "sourceMode": "external_providers", "attemptCount": 1, "quarantinedAt": "2026-10-01T00:00:00.000Z", "schemaVersion": 1, "quarantineReason": "l1_rollback_beyond_finality", "stateQueueOutRef": "a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3#0", "deploymentFingerprint": "abababababababababababababababababababababababababababababababab"}', '2026-10-07 23:49:23.38861-05', '2026-10-07 23:49:23.38861-05');


--
-- Data for Name: committee_deployment; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_deployment VALUES (1, 'midgard-deployment-marker-v1', 'abababababababababababababababababababababababababababababababab', 'fee5b83d46255252709b52737bede8b5eae0864318e786db054a409d182d64f1', '0000000000000000000000000000000000000000000000000000000000000000', '{"da":{"committeeSignersHash":"7f26c5a0c2219f58abcbc2ebd2da349acb10773ffbc37b6af91fa8df2486c9ea","transportProfile":{"retentionDays":15}}}', '2026-10-07 23:49:23.192641-05', '2026-10-07 23:49:23.192641-05');


--
-- Data for Name: committee_l1_source_state; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_l1_source_state VALUES (1, '{"status": "healthy", "network": "Preprod", "observedAt": "2026-10-02T00:00:00.000Z", "sourceMode": "local_node", "observations": [{"slot": 100, "blockHash": "1212121212121212121212121212121212121212121212121212121212121212", "finalized": true, "headerHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "stateQueueOutRef": "6666666666666666666666666666666666666666666666666666666666666666#1", "stateQueueStatus": "removed", "hasPersistedDecision": true}], "schemaVersion": 1, "authoritySha256": "efefefefefefefefefefefefefefefefefefefefefefefefefefefefefefefef"}', '2026-10-07 23:49:23.197608-05', '2026-10-07 23:49:23.354488-05');


--
-- Data for Name: committee_l1_submissions; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_l1_submissions VALUES ('73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0', 'apply', '8888888888888888888888888888888888888888888888888888888888888888', '{"txHash": "8888888888888888888888888888888888888888888888888888888888888888", "txKind": "apply", "headerHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "inputsUsed": [], "submittedAt": "2026-10-02T00:00:00.000Z", "resultStatus": "confirmed", "deploymentFingerprint": "abababababababababababababababababababababababababababababababab"}', '2026-10-07 23:49:23.329574-05', '2026-10-07 23:49:23.329574-05');


--
-- Data for Name: committee_peer_broadcasts; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_peer_broadcasts VALUES ('peer1', '73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0', 'c81c0e2d0483b9417a88ec445d6662261a78ca4c7b22e0252671f0b7400d8c1f', 0, '{"peerId": "peer1", "status": "posted", "attempts": 1, "updatedAt": "2026-10-02T00:00:00.000Z", "headerHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "signerIndex": 0, "deploymentFingerprint": "abababababababababababababababababababababababababababababababab", "availabilityCommitmentDigest": "c81c0e2d0483b9417a88ec445d6662261a78ca4c7b22e0252671f0b7400d8c1f"}', '2026-10-07 23:49:23.342499-05', '2026-10-07 23:49:23.342499-05');


--
-- Data for Name: committee_peer_health; Type: TABLE DATA; Schema: public; Owner: -
--



--
-- Data for Name: committee_peer_nonces; Type: TABLE DATA; Schema: public; Owner: -
--



--
-- Data for Name: committee_promise_capacity_evidence; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_promise_capacity_evidence VALUES ('9f0058d61ff80891281259c4f4a56240a9f9e9677774e496aec1ca7094245165', '{"point": {"slot": 100, "blockNo": 100, "blockHash": "1212121212121212121212121212121212121212121212121212121212121212"}, "actorId": "8b218424ad74df25d35c2ea8e094a4c5c5aeb2cbb442419331569313", "headerHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "certifiedAt": {"slot": 2000000, "blockNo": 3000, "blockHash": "3434343434343434343434343434343434343434343434343434343434343434"}, "cutoffTimeMs": 720001, "recoveryDepth": 2160, "retirementKind": "terminal", "commitmentDigest": "c81c0e2d0483b9417a88ec445d6662261a78ca4c7b22e0252671f0b7400d8c1f", "contractManifestId": "8989898989898989898989898989898989898989898989898989898989898989", "deploymentFingerprint": "abababababababababababababababababababababababababababababababab"}');


--
-- Data for Name: committee_retirement_metadata; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_retirement_metadata VALUES (1, '{"digest": "8bd373eba52db5b8af26f21cf5085551ed02d7ed88d072ecfea965601aa23756", "binding": {"actorId": "8b218424ad74df25d35c2ea8e094a4c5c5aeb2cbb442419331569313", "peerIds": ["peer1"], "recoveryDepth": 2160, "retentionDays": 15, "manifestSha256": "fee5b83d46255252709b52737bede8b5eae0864318e786db054a409d182d64f1", "maximumRecords": 512, "contractManifestId": "8989898989898989898989898989898989898989898989898989898989898989", "maximumEncodedBytes": 8388608, "committeeSignersHash": "7f26c5a0c2219f58abcbc2ebd2da349acb10773ffbc37b6af91fa8df2486c9ea", "deploymentFingerprint": "abababababababababababababababababababababababababababababababab", "sourceAuthoritySha256": "efefefefefefefefefefefefefefefefefefefefefefefefefefefefefefefef"}, "generation": 1, "schemaVersion": 1, "nonceTimeFloorMs": 0}');


--
-- Data for Name: committee_state_queue_headers; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_state_queue_headers VALUES ('73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0', '{"header": {"endTime": {"value": "1", "__midgardWatcherType": "bigint"}, "minFeeA": {"value": "0", "__midgardWatcherType": "bigint"}, "minFeeB": {"value": "0", "__midgardWatcherType": "bigint"}, "blockSlot": {"value": "0", "__midgardWatcherType": "bigint"}, "startTime": {"value": "0", "__midgardWatcherType": "bigint"}, "utxosRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8", "depositCount": {"value": "0", "__midgardWatcherType": "bigint"}, "depositsRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8", "operatorVkey": "22222222222222222222222222222222222222222222222222222222", "prevUtxosRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8", "prevHeaderHash": "00000000000000000000000000000000000000000000000000000000", "eventToStepRoot": "6ccb9c11a4bec000df61ad38bbf071a5687d57d3254ff4b172ca8aa686a4ac11", "protocolVersion": {"value": "1", "__midgardWatcherType": "bigint"}, "totalEventCount": {"value": "1", "__midgardWatcherType": "bigint"}, "withdrawalCount": {"value": "0", "__midgardWatcherType": "bigint"}, "withdrawalsRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8", "transactionsRoot": "a713c7c168f3b744260b1c19513d968d95f284f13db85681f3a4a3386b50f476", "expectedNetworkId": {"value": "0", "__midgardWatcherType": "bigint"}, "l2TransactionCount": {"value": "1", "__midgardWatcherType": "bigint"}, "transitionStepCount": {"value": "1", "__midgardWatcherType": "bigint"}, "transitionTraceRoot": "2b11d07988496b21835ee3acb3f13086355f2202f907951f63047b4604595393", "validationTraceCount": {"value": "1", "__midgardWatcherType": "bigint"}, "validationTracesRoot": "c5647c7428e49410fa02e263cbffcebff4abd15b597a33bc812e2274bc68d0ed", "forcedTransactionCount": {"value": "0", "__midgardWatcherType": "bigint"}, "forcedTransactionsRoot": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"}, "status": "removed", "finalized": true, "updatedAt": "2026-10-02T00:00:00.000Z", "headerHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "daAttestation": "Unattested", "blockAssetName": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "stateQueueOutRef": "6666666666666666666666666666666666666666666666666666666666666666#1", "validationErrors": [], "computedHeaderHash": "73ccf751aca9a6b8fa6ead42cf7d40c644b9e496de29e9e26bf49be0", "observedChainPoint": {"slot": 100, "depth": 5000, "blockHash": "1212121212121212121212121212121212121212121212121212121212121212", "finalized": true, "blockHeight": 100, "providerSource": "authenticated_state_queue_transition_v1"}, "deploymentFingerprint": "abababababababababababababababababababababababababababababababab"}', '2026-10-07 23:49:23.236235-05', '2026-10-07 23:49:23.236235-05');


--
-- Data for Name: committee_store_counts; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.committee_store_counts VALUES ('headers', 1);
INSERT INTO public.committee_store_counts VALUES ('verified_payloads', 1);
INSERT INTO public.committee_store_counts VALUES ('signatures', 1);
INSERT INTO public.committee_store_counts VALUES ('l1_submissions', 1);
INSERT INTO public.committee_store_counts VALUES ('submitted_or_confirmed_headers', 1);


--
-- Name: committee_da_attestation_candidates committee_da_attestation_candidates_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_da_attestation_candidates
    ADD CONSTRAINT committee_da_attestation_candidates_pkey PRIMARY KEY (header_hash, out_ref);


--
-- Name: committee_da_conflict_evidence committee_da_conflict_evidence_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_da_conflict_evidence
    ADD CONSTRAINT committee_da_conflict_evidence_pkey PRIMARY KEY (deployment_fingerprint, evidence_hash);


--
-- Name: committee_da_payloads committee_da_payloads_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_da_payloads
    ADD CONSTRAINT committee_da_payloads_pkey PRIMARY KEY (header_hash);


--
-- Name: committee_da_signatures committee_da_signatures_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_da_signatures
    ADD CONSTRAINT committee_da_signatures_pkey PRIMARY KEY (header_hash, commitment_digest, signer_index);


--
-- Name: committee_decision_outbox committee_decision_outbox_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_decision_outbox
    ADD CONSTRAINT committee_decision_outbox_pkey PRIMARY KEY (effect_id);


--
-- Name: committee_deployment committee_deployment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_deployment
    ADD CONSTRAINT committee_deployment_pkey PRIMARY KEY (id);


--
-- Name: committee_l1_source_state committee_l1_source_state_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_l1_source_state
    ADD CONSTRAINT committee_l1_source_state_pkey PRIMARY KEY (id);


--
-- Name: committee_l1_submissions committee_l1_submissions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_l1_submissions
    ADD CONSTRAINT committee_l1_submissions_pkey PRIMARY KEY (header_hash, tx_kind, tx_hash);


--
-- Name: committee_peer_broadcasts committee_peer_broadcasts_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_peer_broadcasts
    ADD CONSTRAINT committee_peer_broadcasts_pkey PRIMARY KEY (peer_id, header_hash, commitment_digest, signer_index);


--
-- Name: committee_peer_health committee_peer_health_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_peer_health
    ADD CONSTRAINT committee_peer_health_pkey PRIMARY KEY (peer_id);


--
-- Name: committee_peer_nonces committee_peer_nonces_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_peer_nonces
    ADD CONSTRAINT committee_peer_nonces_pkey PRIMARY KEY (deployment_fingerprint, signer_index, nonce);


--
-- Name: committee_promise_capacity_evidence committee_promise_capacity_evidence_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_promise_capacity_evidence
    ADD CONSTRAINT committee_promise_capacity_evidence_pkey PRIMARY KEY (evidence_key);


--
-- Name: committee_retirement_metadata committee_retirement_metadata_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_retirement_metadata
    ADD CONSTRAINT committee_retirement_metadata_pkey PRIMARY KEY (id);


--
-- Name: committee_state_queue_headers committee_state_queue_headers_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_state_queue_headers
    ADD CONSTRAINT committee_state_queue_headers_pkey PRIMARY KEY (header_hash);


--
-- Name: committee_store_counts committee_store_counts_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.committee_store_counts
    ADD CONSTRAINT committee_store_counts_pkey PRIMARY KEY (name);


--
-- Name: committee_da_signatures_signed_decisions; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX committee_da_signatures_signed_decisions ON public.committee_da_signatures USING btree (header_hash) WHERE (end_time_ms IS NOT NULL);


--
-- Name: committee_state_queue_headers_open; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX committee_state_queue_headers_open ON public.committee_state_queue_headers USING btree (header_hash) WHERE ((record ->> 'status'::text) = ANY (ARRAY['unattested'::text, 'attesting'::text]));


--
-- Name: committee_state_queue_headers_unsettled; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX committee_state_queue_headers_unsettled ON public.committee_state_queue_headers USING btree (header_hash) WHERE ((record ->> 'status'::text) <> ALL (ARRAY['merged'::text, 'removed'::text]));


--
-- Name: committee_da_payloads committee_da_payloads_verified_count_delete; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_da_payloads_verified_count_delete AFTER DELETE ON public.committee_da_payloads REFERENCING OLD TABLE AS old_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_verified_payloads();


--
-- Name: committee_da_payloads committee_da_payloads_verified_count_insert; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_da_payloads_verified_count_insert AFTER INSERT ON public.committee_da_payloads REFERENCING NEW TABLE AS new_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_verified_payloads();


--
-- Name: committee_da_payloads committee_da_payloads_verified_count_update; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_da_payloads_verified_count_update AFTER UPDATE ON public.committee_da_payloads REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_verified_payloads();


--
-- Name: committee_da_signatures committee_da_signatures_count_delete; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_da_signatures_count_delete AFTER DELETE ON public.committee_da_signatures REFERENCING OLD TABLE AS old_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_rows('signatures');


--
-- Name: committee_da_signatures committee_da_signatures_count_insert; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_da_signatures_count_insert AFTER INSERT ON public.committee_da_signatures REFERENCING NEW TABLE AS new_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_rows('signatures');


--
-- Name: committee_l1_submissions committee_l1_submissions_count_delete; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_l1_submissions_count_delete AFTER DELETE ON public.committee_l1_submissions REFERENCING OLD TABLE AS old_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_rows('l1_submissions');


--
-- Name: committee_l1_submissions committee_l1_submissions_count_insert; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_l1_submissions_count_insert AFTER INSERT ON public.committee_l1_submissions REFERENCING NEW TABLE AS new_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_rows('l1_submissions');


--
-- Name: committee_l1_submissions committee_l1_submissions_submitted_count_delete; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_l1_submissions_submitted_count_delete AFTER DELETE ON public.committee_l1_submissions REFERENCING OLD TABLE AS old_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_submitted_headers();


--
-- Name: committee_l1_submissions committee_l1_submissions_submitted_count_insert; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_l1_submissions_submitted_count_insert AFTER INSERT ON public.committee_l1_submissions REFERENCING NEW TABLE AS new_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_submitted_headers();


--
-- Name: committee_l1_submissions committee_l1_submissions_submitted_count_update; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_l1_submissions_submitted_count_update AFTER UPDATE ON public.committee_l1_submissions REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_submitted_headers();


--
-- Name: committee_state_queue_headers committee_state_queue_headers_count_delete; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_state_queue_headers_count_delete AFTER DELETE ON public.committee_state_queue_headers REFERENCING OLD TABLE AS old_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_rows('headers');


--
-- Name: committee_state_queue_headers committee_state_queue_headers_count_insert; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER committee_state_queue_headers_count_insert AFTER INSERT ON public.committee_state_queue_headers REFERENCING NEW TABLE AS new_rows FOR EACH STATEMENT EXECUTE FUNCTION public.committee_store_count_rows('headers');


--
-- PostgreSQL database dump complete
--


