import { randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { submitDepositWithMetadataProgram } from "../src/transactions/submit-deposit.js";
import { outRefLabel } from "../src/tx-context.js";
import {
  describeProviderOutRefStates,
  type EmulatorFixture,
  isEmulatorProvider,
  isProviderVisibleUnspent,
  runNodeDatabaseEffect,
} from "./deposit-flow-emulator-shared.make-fixture.js";
import { collectSortedInputOutRefs } from "./helpers/tx-inspection.js";

export const refreshWalletUtxosFromProvider = async (
  lucid: LucidEvolution,
): Promise<void> => {
  const overrideUTxOs = (
    lucid as LucidEvolution & { overrideUTxOs?: (utxos: UTxO[]) => void }
  ).overrideUTxOs;
  if (typeof overrideUTxOs !== "function") {
    return;
  }
  const walletAddress = await lucid.wallet().address();
  const provider = lucid.config().provider as {
    readonly ledger?: Record<
      string,
      { readonly utxo?: UTxO; readonly spent?: boolean } | undefined
    >;
    readonly mempool?: Record<
      string,
      { readonly utxo?: UTxO; readonly spent?: boolean } | undefined
    >;
  };
  const visibleProviderEntries = [
    ...Object.values(provider.ledger ?? {}),
    ...Object.values(provider.mempool ?? {}),
  ];
  const walletUtxos =
    visibleProviderEntries.length > 0
      ? visibleProviderEntries.flatMap((entry) => {
          if (
            entry === undefined ||
            entry.spent === true ||
            entry.utxo === undefined ||
            entry.utxo.address !== walletAddress
          ) {
            return [];
          }
          return [entry.utxo];
        })
      : (await lucid.utxosAt(walletAddress)).filter((utxo) =>
          isProviderVisibleUnspent(lucid, utxo),
        );
  overrideUTxOs.call(lucid, walletUtxos);
};

export const providerVisibleWalletUtxos = async (
  lucid: LucidEvolution,
): Promise<UTxO[]> => {
  const walletAddress = await lucid.wallet().address();
  const provider = lucid.config().provider as {
    readonly ledger?: Record<
      string,
      { readonly utxo?: UTxO; readonly spent?: boolean } | undefined
    >;
    readonly mempool?: Record<
      string,
      { readonly utxo?: UTxO; readonly spent?: boolean } | undefined
    >;
  };
  const visibleProviderEntries = [
    ...Object.values(provider.ledger ?? {}),
    ...Object.values(provider.mempool ?? {}),
  ];
  if (visibleProviderEntries.length > 0) {
    return visibleProviderEntries.flatMap((entry) => {
      if (
        entry === undefined ||
        entry.spent === true ||
        entry.utxo === undefined ||
        entry.utxo.address !== walletAddress
      ) {
        return [];
      }
      return [entry.utxo];
    });
  }
  return (await lucid.utxosAt(walletAddress)).filter((utxo) =>
    isProviderVisibleUnspent(lucid, utxo),
  );
};

export const isPlainPureAdaUtxo = (utxo: UTxO): boolean =>
  utxo.scriptRef === undefined &&
  Object.entries(utxo.assets).every(
    ([unit, quantity]) => unit === "lovelace" || quantity === 0n,
  );

export const ensureSeparateCollateralUtxo = async (
  lucid: LucidEvolution,
): Promise<void> => {
  await refreshWalletUtxosFromProvider(lucid);
  const walletAddress = await lucid.wallet().address();
  const pureAdaUtxos = (await providerVisibleWalletUtxos(lucid))
    .filter(isPlainPureAdaUtxo)
    .filter((utxo) => (utxo.assets.lovelace ?? 0n) >= 20_000_000n)
    .sort((left, right) => {
      const leftLovelace = left.assets.lovelace ?? 0n;
      const rightLovelace = right.assets.lovelace ?? 0n;
      if (leftLovelace === rightLovelace) {
        return outRefLabel(left).localeCompare(outRefLabel(right));
      }
      return leftLovelace > rightLovelace ? -1 : 1;
    });
  if (pureAdaUtxos.length >= 2) {
    return;
  }
  const source = pureAdaUtxos[0];
  if (source === undefined) {
    throw new Error("Operator wallet has no pure ADA UTxO to split");
  }
  const splitTx = await lucid
    .newTx()
    .collectFrom([source])
    .pay.ToAddress(walletAddress, { lovelace: 8_000_000n })
    .pay.ToAddress(walletAddress, { lovelace: 8_000_000n })
    .addSigner(walletAddress)
    .complete({ localUPLCEval: true });
  await submitWithWallet(lucid, splitTx);
  await refreshWalletUtxosFromProvider(lucid);
};

export const submitWithWallet = async (
  lucid: LucidEvolution,
  tx: TxSignBuilder,
): Promise<string> => {
  await refreshWalletUtxosFromProvider(lucid);
  const signed = await tx.sign.withWallet().complete();
  const txHash = signed.toHash();
  const signedTx = CML.Transaction.from_cbor_hex(signed.toCBOR());
  const signedInputs = collectSortedInputOutRefs(signedTx.body().inputs()).map(
    outRefLabel,
  );
  const signedReferenceInputs =
    signedTx.body().reference_inputs() === undefined
      ? []
      : collectSortedInputOutRefs(signedTx.body().reference_inputs()!).map(
          outRefLabel,
        );
  const signedCollateralInputs =
    signedTx.body().collateral_inputs() === undefined
      ? []
      : collectSortedInputOutRefs(signedTx.body().collateral_inputs()!).map(
          outRefLabel,
        );
  const plutusV3Scripts = signedTx.witness_set().plutus_v3_scripts();
  const witnessHashes =
    plutusV3Scripts === undefined
      ? []
      : Array.from({ length: Number(plutusV3Scripts.len()) }, (_value, index) =>
          plutusV3Scripts.get(index).hash().to_hex(),
        );
  const result = await signed.submitSafe();
  if (result._tag === "Left") {
    const provider = lucid.config().provider;
    const extraneousScriptHash = result.left.message.match(
      /Extraneous plutus script\. Script hash: ([0-9a-fA-F]{56})/,
    )?.[1];
    if (extraneousScriptHash !== undefined && isEmulatorProvider(provider)) {
      const emulatorCompatibleTxCbor = stripPlutusV3WitnessByHash({
        txCbor: signed.toCBOR(),
        witnessHash: extraneousScriptHash.toLowerCase(),
      });
      const submittedHash = await provider.submitTx(emulatorCompatibleTxCbor);
      await lucid.awaitTx(submittedHash);
      return submittedHash;
    }
    throw new Error(
      [
        `Reserve/payout submission failed for tx=${txHash}`,
        `provider_error=${result.left.message}`,
        `signed_inputs=${signedInputs.join(",")}`,
        `signed_reference_inputs=${signedReferenceInputs.join(",")}`,
        `signed_collateral_inputs=${signedCollateralInputs.join(",")}`,
        `provider_input_states=${JSON.stringify(
          describeProviderOutRefStates(lucid, signedInputs),
        )}`,
        `provider_ref_states=${JSON.stringify(
          describeProviderOutRefStates(lucid, signedReferenceInputs),
        )}`,
        `provider_collateral_states=${JSON.stringify(
          describeProviderOutRefStates(lucid, signedCollateralInputs),
        )}`,
        `tx_cbor_bytes=${signed.toCBOR().length / 2}`,
        `plutus_v3_witness_hashes=${witnessHashes.join(",")}`,
      ].join("\n"),
    );
  }
  await lucid.awaitTx(result.right);
  return result.right;
};

export const stripPlutusV3WitnessByHash = ({
  txCbor,
  witnessHash,
}: {
  readonly txCbor: string;
  readonly witnessHash: string;
}): string => {
  const tx = CML.Transaction.from_cbor_hex(txCbor);
  const witnessSet = tx.witness_set();
  const scripts = witnessSet.plutus_v3_scripts();
  if (scripts === undefined) {
    throw new Error(
      `Failed to strip emulator-only witness workaround; tx has no Plutus V3 witnesses for hash=${witnessHash}`,
    );
  }

  const filteredScripts = CML.PlutusV3ScriptList.new();
  let removed = 0;
  for (let index = 0; index < scripts.len(); index += 1) {
    const script = scripts.get(index);
    if (script.hash().to_hex() === witnessHash) {
      removed += 1;
      continue;
    }
    filteredScripts.add(script);
  }
  if (removed !== 1) {
    throw new Error(
      `Expected to remove exactly one emulator-only cert witness for hash=${witnessHash}, removed=${removed.toString()}`,
    );
  }

  witnessSet.set_plutus_v3_scripts(filteredScripts);
  return CML.Transaction.new(
    tx.body(),
    witnessSet,
    tx.is_valid(),
    tx.auxiliary_data(),
  ).to_cbor_hex();
};

/** Advance the isolated ledger past actual pointer protection before freezing
 * Date for a synchronous emulator admission. Real submissions wait on the clock. */
export const advanceHistoryAdmissionClock = async (
  fixture: EmulatorFixture,
  kind: "deposit" | "withdrawal",
) => {
  const deployment = SDK.eventHistoryDeploymentFromContracts(
    SDK.requireEventHistoryContracts(fixture.contracts)[kind],
  );
  const nodes = SDK.authenticateHistoryNodes(
    await fixture.depositorLucid.utxosAt(deployment.address),
    deployment,
  );
  if (!nodes.some(({ key }) => key === null))
    throw new Error("History admission requires its initialized root");
  const protectedUntil = nodes.reduce(
    (latest, { node }) =>
      node.protected_until > latest ? node.protected_until : latest,
    0n,
  );
  const readyAt = Number(protectedUntil) + 60_000;
  if (!Number.isSafeInteger(readyAt))
    throw new Error("History protection exceeds the emulator clock range");
  await advanceEmulatorPastUnixTime(fixture, readyAt);
  vi.setSystemTime(new Date(fixture.emulator.now()));
};

export const submitDepositWithDiagnostics = async (
  fixture: EmulatorFixture,
  config: {
    readonly l2Address: string;
    readonly l2Datum: string | null;
    readonly lovelace: bigint;
    readonly additionalAssets: Readonly<Record<string, bigint>>;
  },
): Promise<string> => {
  await ensureSeparateCollateralUtxo(fixture.depositorLucid);
  await advanceHistoryAdmissionClock(fixture, "deposit");
  const result = await runNodeDatabaseEffect(
    submitDepositWithMetadataProgram(
      fixture.depositorLucid,
      fixture.contracts,
      { ...config, referenceScripts: fixture.referenceScripts.deposit },
      `emulator-deposit-${randomUUID()}`,
    ),
  );
  return result.txHash;
};

export const advanceEmulatorPastUnixTime = async (
  fixture: Pick<EmulatorFixture, "emulator">,
  unixTimeMs: number,
) => {
  while (fixture.emulator.now() <= unixTimeMs) {
    fixture.emulator.awaitSlot(1);
  }
};
