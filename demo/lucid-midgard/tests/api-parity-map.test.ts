import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

const parityMapPath = fileURLToPath(
  new URL("../../../docs/lucid-midgard/11-api-parity-map.md", import.meta.url),
);

type ParityRow = {
  readonly entry: string;
  readonly status: string;
  readonly decision: string;
};

const allowedStatuses = new Set([
  "same",
  "adapted",
  "planned",
  "midgard-only",
  "unsupported",
  "midgard-only planned",
  "adapted planned",
]);

const parseParityRows = (markdown: string): readonly ParityRow[] =>
  markdown
    .split("\n")
    .filter((line) => line.startsWith("| "))
    .map((line) =>
      line
        .split("|")
        .slice(1, -1)
        .map((cell) => cell.trim()),
    )
    .filter(
      (cells) =>
        cells.length >= 3 &&
        cells[0] !== "---" &&
        cells[0] !== "Lucid entry point" &&
        cells[0] !== "Lucid builder entry point",
    )
    .map(([entry, status, decision]) => ({
      entry,
      status,
      decision,
    }));

const expectedParityRows = [
  "`config`::adapted",
  "`wallet`::same",
  "`walletAddress`::midgard-only",
  "`overrideUTxOs`::adapted",
  "`clearUTxOOverrides`::midgard-only",
  "`switchProvider`::adapted",
  "`newTx`::same",
  "`fromTx`::adapted",
  "`selectWallet.fromSeed`::adapted",
  "`selectWallet.fromPrivateKey`::adapted",
  "`selectWallet.fromAPI`::unsupported",
  "`selectWallet.fromAddress`::adapted",
  "`selectWallet.fromExternalSigner`::midgard-only",
  "`currentSlot`::adapted",
  "`unixTimeToSlot`::unsupported",
  "`utxosAt`::adapted",
  "`utxosAtWithUnit`::adapted",
  "`utxoByUnit`::adapted",
  "`utxosByOutRef`::adapted",
  "`delegationAt`::unsupported",
  "`awaitTx`::same",
  "`awaitTxSafe`::midgard-only",
  "`awaitTxProgram`::midgard-only",
  "`txStatus`::midgard-only",
  "`txStatusSafe`::midgard-only",
  "`txStatusProgram`::midgard-only",
  "`datumOf`::adapted",
  "`metadataOf`::unsupported",
  "`readFrom`::same",
  "`collectFrom`::same",
  "`pay.ToAddress`::same",
  "`pay.ToAddressWithData`::adapted",
  "`pay.ToContract`::same",
  "`pay.ToProtectedAddress`::midgard-only",
  "`addSigner`::same",
  "`addSignerKey`::same",
  "`registerStake`::unsupported",
  "`deRegisterStake`::unsupported",
  "`withdraw`::unsupported",
  "`register.Stake`::unsupported",
  "`register.DRep`::unsupported",
  "`deregister.Stake`::unsupported",
  "`deregister.DRep`::unsupported",
  "`mintAssets`::same",
  "`validFrom`::same",
  "`validTo`::same",
  "`delegateTo`::unsupported",
  "`delegate.*`::unsupported",
  "`registerAndDelegate.*`::unsupported",
  "`updateDRep`::unsupported",
  "`authCommitteeHot`::unsupported",
  "`resignCommitteeHot`::unsupported",
  "`attachMetadata`::unsupported",
  "`attach.Script`::same",
  "`attach.NativeScript`::midgard-only",
  "`attach.Datum`::midgard-only",
  "`attach.SpendingValidator`::adapted",
  "`attach.MintingPolicy`::adapted",
  "`attach.ObserverValidator`::midgard-only",
  "`attach.ReferenceScriptMetadata`::midgard-only",
  "`attachObserverScript`::midgard-only",
  "`attach.CertificateValidator`::unsupported",
  "`attach.WithdrawalValidator`::unsupported",
  "`attach.VoteValidator`::unsupported",
  "`attach.ProposeValidator`::unsupported",
  "`observe`::midgard-only",
  "`receiveRedeemer`::midgard-only",
  "`compose`::adapted",
  "`setMinFee`::adapted",
  "`mint`::midgard-only",
  "`complete`::same",
  "`completeSafe`::adapted",
  "`completeProgram`::adapted",
  "`chain`::adapted",
  "`chainSafe`::adapted",
  "`chainProgram`::adapted",
  "`config`::adapted",
  "`rawConfig`::adapted",
  "`lucidConfig`::unsupported",
  "`getPrograms`::unsupported",
  "`debugSnapshot`::midgard-only",
  "`snapshot`::midgard-only",
  "`sign.withWallet`::adapted",
  "`sign.withPrivateKey`::adapted",
  "`sign.withExternalSigner`::midgard-only",
  "`sign.withWitness`::midgard-only",
  "`sign.withWitnesses`::midgard-only",
  "`partialSign.withWallet`::adapted",
  "`partialSign.withWalletEffect`::adapted",
  "`partialSign.withWalletProgram`::midgard-only",
  "`partialSign.withWalletSafe`::adapted",
  "`partialSign.withPrivateKey`::adapted",
  "`partialSign.withPrivateKeyEffect`::adapted",
  "`partialSign.withPrivateKeyProgram`::midgard-only",
  "`partialSign.withPrivateKeySafe`::adapted",
  "`partialSign.withExternalSigner`::midgard-only",
  "`partialSign.withExternalSignerEffect`::midgard-only",
  "`partialSign.withExternalSignerProgram`::midgard-only",
  "`partialSign.withExternalSignerSafe`::midgard-only",
  "`partialSign.withWitness`::midgard-only",
  "`partialSign.withWitnesses`::midgard-only",
  "`assemble`::adapted",
  "`TxSignBuilder.complete`::adapted",
  "`TxSignBuilder.completeSafe`::adapted",
  "`TxSignBuilder.completeProgram`::adapted",
  "`submit`::adapted",
  "`submitSafe`::adapted",
  "`submitProgram`::adapted",
  "`toCBOR`::adapted",
  "`toTransaction`::unsupported",
  "`toJSON`::adapted",
  "`toHash`::adapted",
  "`CompleteTx.tx`::midgard-only",
  "`CompleteTx.txCbor`::midgard-only",
  "`CompleteTx.txHex`::midgard-only",
  "`CompleteTx.txId`::midgard-only",
  "`CompleteTx.txIdHex`::midgard-only",
  "`CompleteTx.metadata`::midgard-only",
  "`CompleteTx.producedOutputs`::midgard-only",
  "`CompleteTx.producedOutput`::midgard-only",
  "`CompleteTx.sign`::adapted",
  "`CompleteTx.partialSign`::adapted",
  "`CompleteTx.assemble`::adapted",
  "`CompleteTx.toPartialWitnessBundle`::midgard-only",
  "`CompleteTx.toPartialWitnessBundleCbor`::midgard-only",
  "`CompleteTx.validate`::midgard-only",
  "`CompleteTx.validateSafe`::midgard-only",
  "`CompleteTx.validateProgram`::midgard-only",
  "`CompleteTx.submit`::adapted",
  "`CompleteTx.submitSafe`::adapted",
  "`CompleteTx.submitProgram`::adapted",
  "`CompleteTx.status`::midgard-only",
  "`CompleteTx.statusSafe`::midgard-only",
  "`CompleteTx.statusProgram`::midgard-only",
  "`CompleteTx.awaitStatus`::midgard-only",
  "`CompleteTx.awaitStatusSafe`::midgard-only",
  "`CompleteTx.awaitStatusProgram`::midgard-only",
  "`PartiallySignedTx.tx`::midgard-only",
  "`PartiallySignedTx.txCbor`::midgard-only",
  "`PartiallySignedTx.txHex`::midgard-only",
  "`PartiallySignedTx.txId`::midgard-only",
  "`PartiallySignedTx.txIdHex`::midgard-only",
  "`PartiallySignedTx.metadata`::midgard-only",
  "`PartiallySignedTx.assemble`::adapted",
  "`PartiallySignedTx.toPartialWitnessBundle`::midgard-only",
  "`PartiallySignedTx.toPartialWitnessBundleCbor`::midgard-only",
  "`SubmittedTx.tx`::midgard-only",
  "`SubmittedTx.admission`::midgard-only",
  "`SubmittedTx.producedOutputs`::midgard-only",
  "`SubmittedTx.producedOutput`::midgard-only",
  "`SubmittedTx.status`::midgard-only",
  "`SubmittedTx.statusSafe`::midgard-only",
  "`SubmittedTx.statusProgram`::midgard-only",
  "`SubmittedTx.awaitStatus`::midgard-only",
  "`SubmittedTx.awaitStatusSafe`::midgard-only",
  "`SubmittedTx.awaitStatusProgram`::midgard-only",
];

describe("API parity map", () => {
  const markdown = readFileSync(parityMapPath, "utf8");
  const rows = parseParityRows(markdown);

  it("keeps every mapped row explicit and status-controlled", () => {
    expect(rows.length).toBeGreaterThan(0);
    for (const row of rows) {
      expect(row.entry).toMatch(/^`.+`$/);
      expect(allowedStatuses.has(row.status)).toBe(true);
      expect(row.decision.length).toBeGreaterThan(0);
    }
  });

  it("guards the reviewed parity matrix against silent drift", () => {
    expect(rows.map((row) => `${row.entry}::${row.status}`)).toEqual(
      expectedParityRows,
    );
  });

  it("documents the non-Cardano semantics that callers must migrate around", () => {
    expect(markdown).toContain("nativeTx.compact.transactionBodyHash");
    expect(markdown).toContain("Midgard native full transaction bytes");
    expect(markdown).toContain("durable admission metadata");
    expect(markdown).toContain("complete({ localValidation");
    expect(markdown).toContain("GET /protocol-info");
    expect(markdown).toContain("MidgardProvider");
  });
});
