import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import { loadCommitteeConfig, type LoadedCommitteeConfig } from "../config.js";
import { openCommitteeStore } from "../store/factory.js";
import { LocalNodeStateQueueProvider, providerFromConfig } from "./provider.js";
import {
  assertUnsignedL1Incident,
  type L1RecoverySnapshot,
  verifyL1Recovery,
} from "./recovery-incident.js";

const help = `recover-l1-source inspect
recover-l1-source --incident <SHA256> --timeout-ms <positive integer>

Uses the unchanged configured deployment and exclusively leased committee store.
Stop and join the held daemon before running this command; no lease bypass exists.
Only unsigned NEW missing-point incidents without a configured availability journal
or responder, with an existing complete native replay anchor, can be recovered.
Every started read is awaited; timeout refuses writes and is not a latency guarantee.
No cursor acknowledgement, signing, submission, effect retry or payload repair occurs.
All other reasons, protected floors and prior liabilities remain refused.
`;
export type L1RecoveryCommand =
  | Readonly<{ kind: "inspect" }>
  | Readonly<{ kind: "help" }>
  | Readonly<{ kind: "recover"; incident: string; timeoutMs: number }>;
export const parseL1RecoveryCommand = (
  argv: readonly string[],
): L1RecoveryCommand => {
  if (argv.length === 1 && ["--help", "-h"].includes(argv[0]!))
    return { kind: "help" };
  if (argv.length === 1 && argv[0] === "inspect") return { kind: "inspect" };
  if (
    argv.length !== 4 ||
    argv[0] !== "--incident" ||
    argv[2] !== "--timeout-ms" ||
    !/^[0-9a-f]{64}$/u.test(argv[1]!) ||
    !/^[1-9][0-9]*$/u.test(argv[3]!)
  )
    throw new Error("invalid_recovery_arguments");
  const timeoutMs = Number(argv[3]);
  if (!Number.isSafeInteger(timeoutMs))
    throw new Error("invalid_recovery_timeout");
  return { kind: "recover", incident: argv[1]!, timeoutMs };
};
const refusalCode = (error: unknown): string =>
  error instanceof Error && /^[a-z][a-z0-9_]*$/u.test(error.message)
    ? error.message
    : "native_or_store_evidence_unavailable";
const inspect = (
  snapshot: L1RecoverySnapshot,
  config: LoadedCommitteeConfig,
) => {
  const residues: string[] = [];
  try {
    assertUnsignedL1Incident(snapshot);
  } catch (error) {
    residues.push(refusalCode(error));
  }
  if (config.availabilityJournalPath || config.availabilitySubmitterKeySource)
    residues.push("journal_exclusive_inventory_unavailable");
  if (config.l1Source.sourceMode !== "local_node")
    residues.push("complete_local_native_source_required");
  return {
    status: snapshot.data.chainCursor?.status ?? "uninitialized",
    recovery: {
      phase: "not_attempted",
      incident: snapshot.digest,
      reason: snapshot.data.chainCursor?.quarantineReason,
      residues,
      recoverCommand:
        "recover-l1-source --incident <incident> --timeout-ms <positive integer>",
      support: "no_journal_responder_disabled_unsigned_new_missing_point_only",
      repairCommands: [],
      instruction:
        "Unsupported residues require a separately proved operation; inspect/recover never reset them.",
    },
  };
};

export const runL1RecoveryCommand = async (
  argv: readonly string[],
  emit: (value: unknown) => void,
): Promise<number> => {
  const command = parseL1RecoveryCommand(argv);
  if (command.kind === "help") {
    emit(help);
    return 0;
  }
  const config = await loadCommitteeConfig();
  const store = await openCommitteeStore(config.localState);
  try {
    const snapshot = await store.readL1RecoverySnapshot();
    if (command.kind === "inspect") {
      emit(inspect(snapshot, config));
      return 0;
    }
    const scope = createDaAvailabilityReadScope({
      attemptTimeoutMs: command.timeoutMs,
    });
    let applying = false;
    try {
      if (command.incident !== snapshot.digest)
        throw new Error("recovery_incident_changed");
      emit({
        status: "quarantined",
        recovery: { phase: "recovering", incident: command.incident },
      });
      assertUnsignedL1Incident(snapshot);
      if (
        config.availabilityJournalPath ||
        config.availabilitySubmitterKeySource
      )
        throw new Error("journal_exclusive_inventory_unavailable");
      if (config.l1Source.sourceMode !== "local_node")
        throw new Error("complete_local_native_source_required");
      const provider = await providerFromConfig(config);
      scope.assertCurrent();
      if (!(provider instanceof LocalNodeStateQueueProvider))
        throw new Error("complete_local_native_source_required");
      const certificate = await verifyL1Recovery({
        snapshot,
        config,
        provider,
        scope,
      });
      applying = true;
      await store.applyL1RecoveryCertificate(certificate);
      emit({
        status: "healthy",
        recovery: { phase: "recovered", incident: command.incident },
      });
      return 0;
    } catch (error) {
      emit({
        status: applying
          ? "unknown"
          : (snapshot.data.chainCursor?.status ?? "uninitialized"),
        recovery: {
          phase: "recovery_refused",
          incident: command.incident,
          code: refusalCode(error),
          inspectRequired: applying,
        },
      });
      return 78;
    } finally {
      scope.close();
    }
  } finally {
    await store.close?.();
  }
};
