import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const read = (path) => readFileSync(join(root, path), "utf8");
const temporaryRoot = process.platform === "win32" ? tmpdir() : "/tmp";

const run = (
  command,
  args,
  { env = process.env, input, timeoutMs = 5_000 } = {},
) =>
  new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      env,
      stdio: ["pipe", "pipe", "pipe"],
    });
    let stdout = "";
    let stderr = "";
    let timedOut = false;
    child.stdout.setEncoding("utf8");
    child.stderr.setEncoding("utf8");
    child.stdout.on("data", (chunk) => {
      stdout += chunk;
    });
    child.stderr.on("data", (chunk) => {
      stderr += chunk;
    });
    const timeout = setTimeout(() => {
      timedOut = true;
      child.kill("SIGKILL");
    }, timeoutMs);
    child.on("error", (error) => {
      clearTimeout(timeout);
      reject(error);
    });
    child.on("close", (status, signal) => {
      clearTimeout(timeout);
      if (timedOut) {
        reject(
          new Error(
            `${command} timed out after ${timeoutMs}ms\nstdout:\n${stdout}\nstderr:\n${stderr}`,
          ),
        );
        return;
      }
      resolve({ status, signal, stdout, stderr });
    });
    if (input === undefined) child.stdin.end();
    else child.stdin.end(input);
  });

test("all shell assets parse", async () => {
  for (const name of [
    "common.sh",
    "generate.sh",
    "bootstrap.sh",
    "fund-wallets.sh",
    "protocol-bootstrap.sh",
    "phas-registration-preflight.sh",
    "write-acceptance-env.sh",
    "capture-snapshot.sh",
    "reset.sh",
    "t1-recover.sh",
    "validate-custom-chain-config.sh",
  ]) {
    const result = await run("sh", ["-n", join(root, "scripts", name)]);
    assert.equal(result.status, 0, `${name}: ${result.stderr}`);
  }
});

test("live acceptance executes the snapshot-bound distribution without rebuilding it", () => {
  const packageJson = JSON.parse(read("../../package.json"));
  assert.equal(
    packageJson.scripts["accept:phase4:pipelined-process"],
    "node dist/index.js e2e-pipelined-commit-process-acceptance",
  );
  assert.match(
    read("README.md"),
    /Snapshot capture records[\s\S]*Reset performs every source, distribution, image, configuration, snapshot/,
  );
});

test("bootstrap explicitly seeds and preflights isolated A/B L2 genesis state", () => {
  const protocolBootstrap = read("scripts/protocol-bootstrap.sh");
  const writeAcceptanceEnv = read("scripts/write-acceptance-env.sh");
  const acceptance = read(
    "../../src/commands/e2e-pipelined-commit-process-acceptance.ts",
  );
  const index = read("../../src/index.ts");

  assert.match(protocolBootstrap, /RUN_GENESIS_ON_STARTUP=false/);
  assert.match(protocolBootstrap, /MIDGARD_DOTENV_MODE=disabled/);
  assert.match(protocolBootstrap, /MIN_FEE_A=0 MIN_FEE_B=0/);
  assert.match(protocolBootstrap, /upsert_private_env MIN_FEE_A 0/);
  assert.match(protocolBootstrap, /upsert_private_env MIN_FEE_B 0/);
  assert.match(
    protocolBootstrap,
    /MIDGARD_PHASE4_GENESIS_BOOTSTRAP=phase4-local-devnet-l2-genesis-v1[\s\S]*phase4-genesis-ledger --seed/,
  );
  assert.doesNotMatch(protocolBootstrap, /submit-deposit/);
  assert.match(
    protocolBootstrap,
    /TESTNET_GENESIS_WALLET_SEED_PHRASE_C=.*TESTNET_GENESIS_WALLET_SEED_PHRASE_A/,
  );
  assert.match(writeAcceptanceEnv, /MIDGARD_DOTENV_MODE: "disabled"/);
  assert.match(writeAcceptanceEnv, /MIN_FEE_A: "0"/);
  assert.match(writeAcceptanceEnv, /MIN_FEE_B: "0"/);
  assert.match(
    writeAcceptanceEnv,
    /values\.TESTNET_GENESIS_WALLET_SEED_PHRASE_C = values\.TESTNET_GENESIS_WALLET_SEED_PHRASE_A/,
  );
  assert.match(acceptance, /phase4-genesis-ledger", "--verify-only/);
  assert.match(acceptance, /PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE/);
  assert.match(index, /loadRuntimeDotenv\(\)/);
});

test("machine-readable reset and recovery keep successful Compose progress off their output streams", () => {
  const common = read("scripts/common.sh");
  assert.match(
    common,
    /compose_quiet\(\)[\s\S]*compose "\$@" >"\$compose_log" 2>&1[\s\S]*cat "\$compose_log" >&2/,
  );
  for (const script of ["reset.sh", "t1-recover.sh"]) {
    const source = read(`scripts/${script}`);
    assert.match(source, /compose_quiet stop/);
    assert.match(source, /compose_quiet up/);
    assert.match(source, /compose_quiet restart ogmios/);
    assert.match(source, /compose_quiet restart kupo/);
    assert.doesNotMatch(source, /(?:^|\s)compose (?:stop|up|restart)/m);
  }
});

test("T1 recovery restores only chain/index and emits exact snapshot-bound evidence", () => {
  const recovery = read("scripts/t1-recover.sh");
  const acceptance = read(
    "../../src/commands/e2e-pipelined-commit-process-acceptance.ts",
  );
  const command = read("../../src/commands/phase4-t1-recovery.ts");
  assert.equal(
    statSync(join(root, "scripts/t1-recover.sh")).mode & 0o777,
    0o755,
  );
  assert.match(recovery, /MIDGARD_PHASE4_PROCESS_TARGET:-.*local-devnet/s);
  assert.match(recovery, /phase4-t1-local-canonical-advance-v1/);
  assert.match(recovery, /run\.env changed the dedicated T1 mutation token/);
  assert.match(recovery, /sha256sum --check SHA256SUMS/);
  assert.match(recovery, /SNAPSHOT_SET_SHA256/);
  assert.match(recovery, /SNAPSHOT_IDENTITY_SHA256/);
  for (const field of [
    "sourceSha256",
    "distSha256",
    "genesisSha256",
    "configSha256",
    "acceptanceEnvSha256",
    "composeSha256",
    "phase4AssetsSha256",
    "phasRegistrationProofSha256",
    "cardanoId",
    "ogmiosId",
    "kupoId",
    "postgresId",
  ])
    assert.match(recovery, new RegExp(field));
  assert.match(recovery, /compose_quiet stop kupo ogmios cardano-node/);
  assert.doesNotMatch(recovery, /compose_quiet stop[^\n]*postgres/);
  assert.match(recovery, /restore_chain_dir cardano-db\.tar\.gz/);
  assert.match(recovery, /restore_chain_dir kupo\.tar\.gz/);
  assert.doesNotMatch(recovery, /restore_chain_dir postgres|postgres\.tar\.gz/);
  assert.match(recovery, /phase4-t1-probe[\s\S]*expected-present-header-hash/);
  assert.match(recovery, /phase4-t1-probe[\s\S]*expected-absent-header-hash/);
  assert.match(recovery, /phase4-t1-advance/);
  assert.match(recovery, /pending_block_finalizations/);
  assert.match(recovery, /pg_dump/);
  assert.match(recovery, /cmp -s .*journal-before\.sql.*journal-after\.sql/);
  assert.match(recovery, /synchronized_slot" = "\$synchronized_kupo"/);
  assert.match(recovery, /midgard-phase4-t1-recovery-attestation-v1/);
  for (const field of [
    "snapshotSetSha256",
    "snapshotIdentitySha256",
    "phasRegistrationProofSha256",
    "phasRegistration",
    "abandonedHeaderHash",
    "abandonedSubmittedTxHash",
    "baseHeaderHash",
    "recoveredTipHeaderHash",
    "canonicalAdvanceTxHash",
    "journalSha256Before",
    "journalSha256After",
    "cardanoTip",
    "kupoCheckpoint",
  ])
    assert.match(recovery, new RegExp(field));

  assert.match(command, /28-byte L2 header hash \(56 lowercase hex\)/);
  assert.match(command, /32-byte Cardano hash \(64 lowercase hex\)/);
  assert.match(command, /commitExplicitBlockHeaderProgram/);
  assert.match(command, /assertPhase4T1NoopAdvance/);
  assert.match(command, /prevHeaderHash !== expectedBase/);
  assert.match(command, /prevUtxosRoot !== before\.canonicalTip\.utxosRoot/);
  assert.match(command, /EMPTY_MERKLE_TREE_ROOT/);

  assert.match(acceptance, /confirmationIntervalMs: 600_000/);
  assert.match(acceptance, /pre-recovery-attempt/);
  assert.match(acceptance, /post-recovery-attempt/);
  assert.match(acceptance, /readLogAttempt/);
  assert.match(acceptance, /parseAndValidatePhase4T1RecoveryAttestation/);
  assert.match(acceptance, /journalByteIdenticalAcrossChainRestore: true/);
  assert.match(acceptance, /replacementPayloadTxIds/);
  assert.match(acceptance, /continuedSpeculation: true/);
  assert.match(
    read("scripts/write-acceptance-env.sh"),
    /key\.startsWith\("MIDGARD_PHASE4_T1_"\)/,
  );
});

test("generator uses era-specific hashes and epoch-zero hardforks", () => {
  const generate = read("scripts/generate.sh");
  assert.match(generate, /byron genesis print-genesis-hash/);
  assert.match(generate, /TestConwayHardForkAtEpoch:0/);
  assert.match(generate, /DijkstraGenesisFile/);
  assert.match(generate, /TestDijkstraHardForkAtEpoch:0/);
  assert.match(generate, /TxSubmissionInitDelay:0/);
  assert.match(generate, /--stake-delegators 1/);
  assert.match(generate, /exactly one registered pool and stake delegation/);
  assert.match(generate, /\.slotLength=1/);
  assert.match(generate, /\.activeSlotsCoeff=1/);
  assert.match(generate, /\.securityParam=90000/);
  assert.match(generate, /\.epochLength=900000/);
  assert.match(generate, /\.protocolConsts\.k=90000/);
  assert.match(generate, /270000 seconds \(75 hours\)/);
  assert.match(generate, /protocolParams\.protocolVersion\.major/);
  assert.match(generate, /validate-custom-chain-config\.sh/);
});

test("isolated bind permissions are explicit and reset-safe", () => {
  const common = read("scripts/common.sh");
  assert.match(read("scripts/generate.sh"), /chmod 0777 .*postgres/);
  assert.match(
    common,
    /cardano_socket="\$MIDGARD_PHASE4_RUN_DIR\/cardano\/ipc\/node\.socket"/,
  );
  assert.match(
    common,
    /attempts=\$\{1:-180\}[\s\S]*while \[ "\$attempts" -gt 0 \][\s\S]*if \[ -S "\$cardano_socket" \] && docker run --rm[\s\S]*return 0[\s\S]*attempts=\$\(\(attempts - 1\)\)[\s\S]*sleep 1/,
  );
  assert.match(
    common,
    /docker run --rm[\s\S]*--volume "\$MIDGARD_PHASE4_RUN_DIR\/cardano\/ipc:\/ipc"[\s\S]*--entrypoint sh "\$PHASE4_POSTGRES_IMAGE"[\s\S]*-ec 'test -S \/ipc\/node\.socket; chmod 0666 \/ipc\/node\.socket'/,
  );
  assert.match(
    common,
    /die "timed out waiting for Cardano node socket access"/,
  );
  assert.match(common, /chmod 0666 \/ipc\/node.socket/);
  assert.match(read("scripts/bootstrap.sh"), /grant_cardano_socket_access/);
  assert.match(read("scripts/reset.sh"), /grant_cardano_socket_access/);
});

test("Kupo Prometheus checkpoint parsing is strict and fail-closed", async () => {
  const common = join(root, "scripts/common.sh");
  const parse = (payload) =>
    run("sh", ["-c", '. "$0"; parse_kupo_checkpoint', common], {
      input: payload,
    });
  const valid = await parse(
    "# HELP kupo_most_recent_checkpoint Latest checkpoint\n" +
      "other_metric 1\n" +
      "kupo_most_recent_checkpoint  6493\n",
  );
  assert.equal(valid.status, 0);
  assert.equal(valid.stdout, "6493\n");

  for (const payload of [
    "other_metric 1\n",
    "kupo_most_recent_checkpoint 1\nkupo_most_recent_checkpoint 2\n",
    'kupo_most_recent_checkpoint{network="devnet"} 6493\n',
    'kupo_most_recent_checkpoint 6493\nkupo_most_recent_checkpoint{network="devnet"} 6493\n',
    "kupo_most_recent_checkpoint 6493.0\n",
    "kupo_most_recent_checkpoint -1\n",
    "kupo_most_recent_checkpoint NaN\n",
    "kupo_most_recent_checkpoint +Inf\n",
  ]) {
    const invalid = await parse(payload);
    assert.notEqual(invalid.status, 0, payload);
    assert.match(
      invalid.stderr,
      /exactly one unlabeled finite nonnegative integer/,
    );
  }

  for (const script of ["capture-snapshot.sh", "reset.sh"]) {
    assert.match(read(`scripts/${script}`), /parse_kupo_checkpoint/);
    assert.doesNotMatch(read(`scripts/${script}`), /mostRecentCheckpoint/);
  }
  assert.match(read("scripts/capture-snapshot.sh"), /kupo-health\.json/);
});

test("latest transaction build omits legacy era selection", () => {
  const funding = read("scripts/fund-wallets.sh");
  assert.doesNotMatch(funding, /transaction build --conway-era/);
});

test("compose is run-scoped and uses isolated default ports", () => {
  const compose = read("compose.yaml");
  assert.match(compose, /MIDGARD_PHASE4_RUN_DIR:\?/);
  assert.match(compose, /MIDGARD_PHASE4_COMPOSE_PROJECT:\?/);
  assert.match(compose, /OGMIOS_PORT:-2337/);
  assert.match(compose, /KUPO_PORT:-2442/);
  assert.match(compose, /POSTGRES_PORT:-5544/);
  assert.match(
    compose,
    /cardanosolutions\/ogmios:v7\.0\.0@sha256:8892ef5f77b94f1c95427cf9f2b40e6235a32b27a8b1e378db02289f3991617f/,
  );
  assert.doesNotMatch(compose, /OGMIOS_IMAGE_TAG|ogmios:\$\{[^}]*latest/);
  assert.match(
    compose,
    /CARDANO_BLOCK_PRODUCER: \$\{MIDGARD_PHASE4_BLOCK_PRODUCER:-true\}/,
  );
  assert.match(compose, /CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE/);
  assert.ok(compose.includes("pools-keys/pool1"));
  assert.doesNotMatch(compose, /delegate-keys\/delegate1/);
  assert.doesNotMatch(compose, /container_name:/);
  for (const image of [
    /ghcr\.io\/intersectmbo\/cardano-node:11\.0\.1@sha256:[a-f0-9]{64}/,
    /cardanosolutions\/kupo:v2\.11\.0@sha256:[a-f0-9]{64}/,
    /postgres:15\.15-alpine@sha256:[a-f0-9]{64}/,
  ]) {
    assert.match(compose, image);
  }
});

test("matched snapshots bind effective images and every build artifact", () => {
  const capture = read("scripts/capture-snapshot.sh");
  assert.match(
    capture,
    /compose restart ogmios[\s\S]*wait_http[^\n]*OGMIOS[\s\S]*compose restart kupo[\s\S]*wait_http[^\n]*KUPO/,
  );
  const reset = read("scripts/reset.sh");
  for (const field of [
    "cardano_image_id",
    "ogmios_image_id",
    "kupo_image_id",
    "postgres_image_id",
    "source_sha",
    "dist_sha",
    "genesis_sha",
    "config_sha",
    "acceptance_env_sha",
    "compose_sha",
  ]) {
    assert.match(capture, new RegExp(field));
  }
  for (const field of [
    "sourceSha256",
    "distSha256",
    "genesisSha256",
    "configSha256",
    "acceptanceEnvSha256",
    "composeSha256",
    "cardanoId",
    "ogmiosId",
    "kupoId",
    "postgresId",
  ])
    assert.match(reset, new RegExp(field));
  for (const archive of ["config.tar.gz", "genesis.tar.gz", "acceptance.env"]) {
    assert.match(capture, new RegExp(archive.replaceAll(".", "\\.")));
    assert.match(reset, new RegExp(archive.replaceAll(".", "\\.")));
  }
});

test("snapshot ownership is explicit before the final permission lock", () => {
  const capture = read("scripts/capture-snapshot.sh");
  const finalArchiveAt = capture.indexOf(
    'archive_dir "$MIDGARD_PHASE4_RUN_DIR/genesis" genesis.tar.gz',
  );
  const checksumsAt = capture.indexOf("sha256sum $snapshot_files >SHA256SUMS");
  const ownershipAt = capture.indexOf("snapshot_uid=$(id -u)");
  const chmodAt = capture.indexOf('chmod -R go-rwx "$snapshot_dir"');
  const successAt = capture.indexOf(
    `printf '%s\\n' "snapshotDir=$snapshot_dir"`,
  );
  assert.ok(finalArchiveAt >= 0 && finalArchiveAt < checksumsAt);
  assert.ok(checksumsAt < ownershipAt && ownershipAt < chmodAt);
  assert.ok(chmodAt < successAt);
  assert.match(capture, /case "\$id_value" in[\s\S]*\*\[!0-9\]\*/);
  assert.match(capture, /require_numeric_id uid "\$snapshot_uid"/);
  assert.match(capture, /require_numeric_id gid "\$snapshot_gid"/);
  assert.match(
    capture,
    /docker run --rm[\s\S]*--volume "\$snapshot_dir:\/snapshot"[\s\S]*--entrypoint sh "\$PHASE4_POSTGRES_IMAGE"[\s\S]*chown -R "\$uid:\$gid" \/snapshot/,
  );
  assert.match(capture, /case "\$uid" in ""\|\*\[!0-9\]\*\) exit 64/);
  assert.match(capture, /case "\$gid" in ""\|\*\[!0-9\]\*\) exit 64/);
  assert.match(capture, /--volume "\$source_dir:\/source:ro"/);
});

test("protocol bootstrap migrates and pins the run-scoped manifest before PHAS", () => {
  const bootstrap = read("scripts/protocol-bootstrap.sh");
  const aikenAt = bootstrap.indexOf("aiken build --env testnet");
  const buildAt = bootstrap.indexOf("pnpm build");
  const migrateAt = bootstrap.indexOf("node dist/index.js db:migrate");
  const deployAt = bootstrap.indexOf(
    "node dist/index.js deploy-reference-script-node-runtime",
  );
  const manifestAt = bootstrap.indexOf(
    'export MIDGARD_DEPLOYMENT_MANIFEST_PATH="$manifest"',
  );
  const contractInfoAt = bootstrap.indexOf(
    'export MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH="$manifest"',
  );
  const phasAt = bootstrap.indexOf(
    "node dist/index.js register-phas-membership-reward-account",
  );
  assert.ok(aikenAt >= 0 && aikenAt < buildAt);
  assert.match(bootstrap, /plutus\.json\.sha256/);
  assert.match(bootstrap, /MIDGARD_REAL_BLUEPRINT_PATH/);
  assert.match(bootstrap, /unset L1_PROVIDER_FAILOVER/);
  assert.match(
    bootstrap,
    /upsert_private_env HUB_ORACLE_ONE_SHOT_TX_HASH "\$tx_hash" "\$node_env"/,
  );
  assert.match(
    bootstrap,
    /upsert_private_env HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX "\$output_index" "\$node_env"/,
  );
  assert.doesNotMatch(bootstrap, /HUB_ORACLE_ONE_SHOT_TX_HASH[^\n]*>>/);
  assert.match(bootstrap, /\. "\$run_env"/);
  assert.match(bootstrap, /authoritative Phase 4 run directory/);
  assert.match(
    bootstrap,
    /unset MIDGARD_DEPLOYMENT_MANIFEST_PATH MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH/,
  );
  assert.ok(buildAt >= 0 && buildAt < migrateAt && migrateAt < deployAt);
  assert.ok(
    deployAt < manifestAt &&
      manifestAt < contractInfoAt &&
      contractInfoAt < phasAt,
  );
  assert.match(bootstrap, /--contract-deployment-info-output "\$manifest"/);
  assert.match(bootstrap, /\.steps\.phasRegistration\.status == "complete"/);
  assert.match(bootstrap, /--registration-transaction-body-output/);
  assert.match(bootstrap, /transactionBody\.artifactSha256/);
  assert.match(
    bootstrap,
    /fresh Phase 4 PHAS registration did not produce exact auditable evidence/,
  );
});

test("PHAS preflight is a pinned read-only ledger and canonical-transaction proof", () => {
  const preflight = read("scripts/phas-registration-preflight.sh");
  assert.match(preflight, /latest query stake-address-info/);
  assert.match(preflight, /address info --address "\$reward_address"/);
  assert.match(preflight, /debug transaction view --output-json/);
  assert.match(preflight, /latest transaction txid/);
  assert.match(preflight, /"f0" \+ \$scriptHash/);
  assert.match(preflight, /Stake address registration/);
  assert.match(preflight, /--socket-path \/run\/cardano\/ipc\/node\.socket/);
  assert.match(preflight, /PHASE4_CARDANO_NODE_IMAGE/);
  assert.match(preflight, /MIDGARD_PHASE4_NETWORK_MAGIC/);
  assert.match(preflight, /matches\/\*%40\$registration_tx_hash/);
  assert.match(preflight, /cardano-cli-local-state-query/);
  assert.match(preflight, /readOnly:true,registered:true/);
  assert.doesNotMatch(
    preflight,
    /rewardAccountSummaries|register-phas|--repair/,
  );

  const capture = read("scripts/capture-snapshot.sh");
  const proofAt = capture.indexOf("phas-registration-preflight.sh");
  const identityAt = capture.indexOf("snapshot-identity.json");
  const checksumsAt = capture.indexOf("sha256sum $snapshot_files");
  assert.ok(proofAt >= 0 && proofAt < identityAt && identityAt < checksumsAt);
  assert.match(capture, /phase4AssetsSha256/);
  assert.match(capture, /phasRegistrationProofSha256/);
  assert.match(capture, /phas-registration-transaction-body\.json/);

  const reset = read("scripts/reset.sh");
  const restoredProofAt = reset.indexOf("phas-registration-preflight.sh");
  const attestationAt = reset.indexOf(
    "midgard-phase4-local-devnet-reset-attestation-v3",
  );
  const resumeAt = reset.indexOf("MIDGARD_PHASE4_BLOCK_PRODUCER=true");
  assert.ok(
    restoredProofAt >= 0 &&
      restoredProofAt < attestationAt &&
      attestationAt < resumeAt,
  );
  assert.match(reset, /cmp -s .*phas-registration-proof\.json/);
});

test("reset rejects every immutable drift before stopping services or restoring durable trees", () => {
  const reset = read("scripts/reset.sh");
  const mutationAt = reset.indexOf("compose_quiet stop kupo ogmios cardano-node postgres");
  const restoreAt = reset.indexOf("restore_dir cardano-db.tar.gz");
  assert.ok(mutationAt >= 0 && mutationAt < restoreAt);
  for (const guard of [
    "snapshot PHAS proof, canonical identity, and transaction body are not exactly bound",
    "current_source_sha=$(tree_sha256",
    "current_dist_sha=$(tree_sha256",
    "current_phase4_assets_sha=$(tree_sha256",
    "effective pinned image IDs do not match",
  ]) {
    const guardAt = reset.indexOf(guard);
    assert.ok(guardAt >= 0 && guardAt < mutationAt, `${guard} must precede mutation`);
  }
  assert.match(reset, /--slurpfile phasRegistration/);
  assert.match(reset, /\.phasRegistration == \$phasRegistration\[0\]/);
  assert.match(reset, /phas-registration-transaction-body\.json/);
});

test("reset attestation binds the complete canonical snapshot identity", () => {
  const reset = read("scripts/reset.sh");
  assert.match(reset, /midgard-phase4-local-devnet-reset-attestation-v3/);
  assert.match(reset, /midgard-phase4-matched-snapshot-identity-v2/);
  assert.match(reset, /restored Cardano\/Kupo state does not match/);
  const observerAt = reset.indexOf(
    "MIDGARD_PHASE4_BLOCK_PRODUCER=false compose_quiet up",
  );
  const attestationAt = reset.indexOf('attestation_path="');
  const producerAt = reset.indexOf(
    "MIDGARD_PHASE4_BLOCK_PRODUCER=true compose_quiet up",
  );
  assert.ok(observerAt >= 0 && observerAt < attestationAt);
  assert.ok(attestationAt < producerAt);
  assert.match(
    reset,
    /Kupo did not catch up to the frozen snapshot checkpoint/,
  );
  const producerSocketAt = reset.indexOf(
    "MIDGARD_PHASE4_BLOCK_PRODUCER=true compose_quiet up",
  );
  const restartOgmiosAt = reset.indexOf(
    "compose_quiet restart ogmios",
    producerSocketAt,
  );
  const waitOgmiosAt = reset.indexOf("OGMIOS_PORT/health", restartOgmiosAt);
  const restartKupoAt = reset.indexOf(
    "compose_quiet restart kupo",
    restartOgmiosAt,
  );
  const waitKupoAt = reset.indexOf("KUPO_PORT/health", restartKupoAt);
  const strictProgressAt = reset.indexOf(
    '[ "$resumed_slot" -gt "$frozen_slot" ] && [ "$resumed_kupo" -gt "$frozen_kupo" ]',
  );
  const publishAt = reset.indexOf(
    'mv "$attestation_pending_path" "$attestation_path"',
  );
  const outputAt = reset.indexOf('cat "$attestation_path"');
  assert.ok(producerSocketAt >= 0);
  assert.ok(producerSocketAt < restartOgmiosAt);
  assert.ok(restartOgmiosAt < waitOgmiosAt && waitOgmiosAt < restartKupoAt);
  assert.ok(restartKupoAt < waitKupoAt && waitKupoAt < strictProgressAt);
  assert.ok(strictProgressAt < publishAt && publishAt < outputAt);
  assert.match(
    reset,
    /rm -f "\$attestation_path" "\$attestation_pending_path"/,
  );
  assert.match(reset, />"\$attestation_pending_path"/);
  assert.doesNotMatch(reset, />"\$attestation_path"/);
  assert.doesNotMatch(reset, /resumed_slot" -ge|resumed_kupo" -ge/);
  assert.match(reset, /did not advance strictly beyond the frozen checkpoint/);
  assert.match(reset, /observed_hash.*frozen_hash/s);
  assert.match(read("scripts/capture-snapshot.sh"), /plutus\.json\.sha256/);
  assert.match(read("scripts/capture-snapshot.sh"), /blueprintSha256/);
  assert.doesNotMatch(reset, /kill "\$pid"/);
  for (const field of [
    "scenarioLabel",
    "composeProject",
    "networkMagic",
    "postgresDatabase",
    "deploymentManifestSha256",
    "snapshotSetSha256",
    "snapshotIdentitySha256",
    "cardanoTip",
    "kupoCheckpoint",
  ])
    assert.match(reset, new RegExp(field));
});

test("crash restart reuses the original durable node identity", () => {
  const acceptance = read(
    "../../src/commands/e2e-pipelined-commit-process-acceptance.ts",
  );
  assert.match(acceptance, /durableStoreLabel/);
  assert.match(acceptance, /exact same node identity and durable MPF stores/);
  assert.match(acceptance, /const crashSpec = makeNodeSpec/);
  assert.match(acceptance, /spec: crashSpec/);
  assert.match(acceptance, /baseTailDatumCbor/);
  assert.match(acceptance, /expectedRoots/);
  assert.match(acceptance, /leaseTokenPresent/);
  assert.match(acceptance, /submittedTxHash/);
});

test("funding and acceptance inputs cover every consumed wallet without seed C", () => {
  const funding = read("scripts/fund-wallets.sh");
  const acceptance = read("scripts/write-acceptance-env.sh");
  assert.match(funding, /funding-confirmation\.ndjson/);
  assert.match(funding, /all-wallet funding did not confirm/);
  assert.doesNotMatch(funding, /TESTNET_GENESIS_WALLET_SEED_PHRASE_C/);
  assert.match(acceptance, /secrets\/acceptance\.env/);
  assert.match(acceptance, /MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath/);
  assert.match(
    acceptance,
    /MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: manifestPath/,
  );
  assert.match(read("scripts/bootstrap.sh"), /write-acceptance-env\.sh/);
  const processAcceptance = read(
    "../../src/commands/e2e-pipelined-commit-process-acceptance.ts",
  );
  assert.doesNotMatch(
    processAcceptance,
    /"reconcile",\s*"phas-registered",\s*"--json"/,
  );
  assert.match(processAcceptance, /snapshotPhasRegistrationProofSha256/);
  assert.match(processAcceptance, /snapshotPhasRegistration/);
  assert.doesNotMatch(
    processAcceptance,
    /\[dist, "register-phas-membership-reward-account"\]/,
  );
});

test("acceptance env is canonical when node.env lacks run-scoped values", async () => {
  const runDir = mkdtempSync(
    join(temporaryRoot, "midgard-phase4-acceptance-env-"),
  );
  mkdirSync(join(runDir, "secrets"), { recursive: true });
  mkdirSync(join(runDir, "deploymentInfo"), { recursive: true });
  writeFileSync(
    join(runDir, "run.env"),
    [
      "MIDGARD_PHASE4_RUN_ID=asset_test",
      `MIDGARD_PHASE4_RUN_DIR=${runDir}`,
      "MIDGARD_PHASE4_COMPOSE_PROJECT=midgard_phase4_process_asset_test",
      "MIDGARD_PHASE4_NETWORK_MAGIC=424242",
      "MIDGARD_PHASE4_OGMIOS_PORT=2337",
      "MIDGARD_PHASE4_KUPO_PORT=2442",
      "MIDGARD_PHASE4_POSTGRES_PORT=5544",
      "MIDGARD_PHASE4_POSTGRES_USER=phase4",
      "MIDGARD_PHASE4_POSTGRES_PASSWORD=test_only",
      "MIDGARD_PHASE4_POSTGRES_DATABASE=midgard_phase4_process_asset_test",
      "",
    ].join("\n"),
  );
  writeFileSync(
    join(runDir, "secrets/node.env"),
    "POSTGRES_HOST=stale\nL1_PROVIDER=Blockfrost\nL1_PROVIDER_FAILOVER=legacy\n",
  );
  writeFileSync(
    join(runDir, "secrets/wallets.env"),
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_A=test-a\nTESTNET_GENESIS_WALLET_SEED_PHRASE_B=test-b\n",
  );
  writeFileSync(
    join(runDir, "deploymentInfo/contract-deployment-info.json"),
    "{}\n",
  );
  const result = await run(
    "sh",
    [join(root, "scripts/write-acceptance-env.sh")],
    {
      env: { ...process.env, MIDGARD_PHASE4_RUN_DIR: runDir },
    },
  );
  assert.equal(result.status, 0, result.stderr);
  const output = readFileSync(join(runDir, "secrets/acceptance.env"), "utf8");
  for (const expected of [
    'NETWORK="Custom"',
    'L1_PROVIDER="Kupmios"',
    'L1_OGMIOS_KEY="http://127.0.0.1:2337"',
    'L1_KUPO_KEY="http://127.0.0.1:2442"',
    'POSTGRES_HOST="127.0.0.1"',
    'POSTGRES_PORT="5544"',
    'POSTGRES_DB="midgard_phase4_process_asset_test"',
    'RUN_GENESIS_ON_STARTUP="false"',
  ]) {
    assert.match(output, new RegExp(`^${expected}$`, "m"));
  }
  assert.doesNotMatch(
    output,
    /Blockfrost|POSTGRES_HOST="stale"|L1_PROVIDER_FAILOVER/,
  );
  rmSync(runDir, { recursive: true, force: true });
});

test("generator refuses an existing run directory before Docker", async () => {
  const existing = mkdtempSync(join(temporaryRoot, "midgard-phase4-existing-"));
  const result = await run("sh", [join(root, "scripts/generate.sh")], {
    env: { ...process.env, MIDGARD_PHASE4_RUN_DIR: existing },
  });
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /refusing to overwrite existing run directory/);
  rmSync(existing, { recursive: true, force: true });
});
test("nonce parser keeps progress transcript and fails closed without final JSON", () => {
  const protocol = read("scripts/protocol-bootstrap.sh");
  assert.ok(protocol.includes("nonce_json="));
  assert.ok(protocol.includes("sed -n"));
  const transcript =
    "[info] progress\n" + JSON.stringify({ txHash: "a".repeat(64) }) + "\n";
  const parsed = JSON.parse(transcript.slice(transcript.indexOf("\n{") + 1));
  assert.equal(parsed.txHash, "a".repeat(64));
  assert.throws(() => JSON.parse("[info] progress\n"));
});
test("bootstrap derives one exact reference address and safely upserts both env keys", () => {
  const bootstrap = read("scripts/bootstrap.sh");
  assert.match(
    bootstrap,
    /expected exactly one funded reference-script wallet address/,
  );
  assert.match(
    bootstrap,
    /L1_REFERENCE_SCRIPT_ADDRESS conflicts with the funded reference-script wallet/,
  );
  assert.match(
    bootstrap,
    /L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS conflicts with the funded reference-script wallet/,
  );
  assert.match(bootstrap, /upsert_private_env L1_REFERENCE_SCRIPT_ADDRESS/);
  assert.match(
    bootstrap,
    /upsert_private_env L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS/,
  );
  assert.match(bootstrap, /chmod 600 /);
});
test("bootstrap canonicalizes run-scoped database values before compose", () => {
  const bootstrap = read("scripts/bootstrap.sh");
  const composeAt = bootstrap.indexOf("compose up --detach");
  assert.ok(composeAt >= 0);
  for (const key of [
    "POSTGRES_HOST",
    "POSTGRES_PORT",
    "POSTGRES_USER",
    "POSTGRES_PASSWORD",
    "POSTGRES_DB",
  ]) {
    const upsertAt = bootstrap.indexOf(`upsert_private_env ${key}`);
    assert.ok(
      upsertAt >= 0 && upsertAt < composeAt,
      `${key} must be canonicalized before compose`,
    );
  }
  assert.ok(bootstrap.includes('index($0, key "=") == 1'));
  assert.ok(bootstrap.includes('END { if (found == 0) print key "=" value }'));
});
