import { describe, expect, it } from "vitest";
import type {
  CompleteTx,
  LucidMidgard,
  MidgardNodeProvider,
  MidgardProvider,
  ObserverValidator,
  PartiallySignedTx,
  ReadFromOptions,
  SpendingValidator,
  MintingPolicy,
  SubmittedTx,
  TrustedReferenceScriptMetadata,
  TxBuilder,
  TxPartialSignBuilder,
} from "../src/index.js";

// These imports are intentionally negative type guards. If lucid-midgard starts
// exporting Cardano/Lucid provider adapters, these @ts-expect-error comments
// become unused and typecheck fails.
// @ts-expect-error lucid-midgard must not export Lucid/Cardano provider adapter.
import type { Blockfrost } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid/Cardano provider adapter.
import type { Maestro } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid/Cardano provider adapter.
import type { Kupmios } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid/Cardano provider adapter.
import type { Koios } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid/Cardano provider adapter.
import type { Emulator } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid emulator account helpers.
import type { EmulatorAccount } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid emulator account helpers.
import type { generateEmulatorAccount } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid emulator account helpers.
import type { generateEmulatorAccountFromPrivateKey } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid provider package surface.
import type { Provider } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid/Cardano provider config.
import type { MaestroConfig } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid/Cardano provider config.
import type { MaestroSupportedNetworks } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid/Cardano provider errors.
import type { KupmiosError } from "../src/index.js";
// @ts-expect-error lucid-midgard must not export Lucid/Cardano provider errors.
import type { KoiosError } from "../src/index.js";

type Expect<T extends true> = T;
type HasKey<T, K extends PropertyKey> = K extends keyof T ? true : false;

type _TopLevelSupported = [
  Expect<HasKey<LucidMidgard, "wallet">>,
  Expect<HasKey<LucidMidgard, "walletAddress">>,
  Expect<HasKey<LucidMidgard, "config">>,
  Expect<HasKey<LucidMidgard, "switchProvider">>,
  Expect<HasKey<LucidMidgard, "overrideUTxOs">>,
  Expect<HasKey<LucidMidgard, "clearUTxOOverrides">>,
  Expect<HasKey<LucidMidgard, "newTx">>,
  Expect<HasKey<LucidMidgard, "fromTx">>,
  Expect<HasKey<LucidMidgard, "currentSlot">>,
  Expect<HasKey<LucidMidgard, "utxosAt">>,
  Expect<HasKey<LucidMidgard, "utxosAtWithUnit">>,
  Expect<HasKey<LucidMidgard, "utxoByUnit">>,
  Expect<HasKey<LucidMidgard, "utxosByOutRef">>,
  Expect<HasKey<LucidMidgard, "datumOf">>,
  Expect<HasKey<LucidMidgard, "txStatus">>,
  Expect<HasKey<LucidMidgard, "txStatusSafe">>,
  Expect<HasKey<LucidMidgard, "txStatusProgram">>,
  Expect<HasKey<LucidMidgard, "awaitTx">>,
  Expect<HasKey<LucidMidgard, "awaitTxSafe">>,
  Expect<HasKey<LucidMidgard, "awaitTxProgram">>,
];

type _BuilderSupported = [
  Expect<HasKey<TxBuilder, "collectFrom">>,
  Expect<HasKey<TxBuilder, "readFrom">>,
  Expect<HasKey<TxBuilder, "addSigner">>,
  Expect<HasKey<TxBuilder, "addSignerKey">>,
  Expect<HasKey<TxBuilder, "mintAssets">>,
  Expect<HasKey<TxBuilder, "mint">>,
  Expect<HasKey<TxBuilder, "observe">>,
  Expect<HasKey<TxBuilder, "attachObserverScript">>,
  Expect<HasKey<TxBuilder, "receiveRedeemer">>,
  Expect<HasKey<TxBuilder, "compose">>,
  Expect<HasKey<TxBuilder, "setMinFee">>,
  Expect<HasKey<TxBuilder, "config">>,
  Expect<HasKey<TxBuilder, "rawConfig">>,
  Expect<HasKey<TxBuilder, "debugSnapshot">>,
  Expect<HasKey<TxBuilder, "snapshot">>,
  Expect<HasKey<TxBuilder, "complete">>,
  Expect<HasKey<TxBuilder, "completeSafe">>,
  Expect<HasKey<TxBuilder, "completeProgram">>,
  Expect<HasKey<TxBuilder, "chain">>,
  Expect<HasKey<TxBuilder, "chainSafe">>,
  Expect<HasKey<TxBuilder, "chainProgram">>,
];

type _CompleteTxSupported = [
  Expect<HasKey<CompleteTx, "sign">>,
  Expect<HasKey<CompleteTx, "partialSign">>,
  Expect<HasKey<CompleteTx, "assemble">>,
  Expect<HasKey<CompleteTx, "toPartialWitnessBundle">>,
  Expect<HasKey<CompleteTx, "toPartialWitnessBundleCbor">>,
  Expect<HasKey<CompleteTx, "toCBOR">>,
  Expect<HasKey<CompleteTx, "toJSON">>,
  Expect<HasKey<CompleteTx, "toHash">>,
  Expect<HasKey<CompleteTx, "producedOutputs">>,
  Expect<HasKey<CompleteTx, "producedOutput">>,
  Expect<HasKey<CompleteTx, "validate">>,
  Expect<HasKey<CompleteTx, "validateSafe">>,
  Expect<HasKey<CompleteTx, "validateProgram">>,
  Expect<HasKey<CompleteTx, "submitSafe">>,
  Expect<HasKey<CompleteTx, "submitProgram">>,
  Expect<HasKey<CompleteTx, "statusSafe">>,
  Expect<HasKey<CompleteTx, "statusProgram">>,
  Expect<HasKey<CompleteTx, "awaitStatusSafe">>,
  Expect<HasKey<CompleteTx, "awaitStatusProgram">>,
];

type _PartialSignSupported = [
  Expect<HasKey<CompleteTx["sign"], "withWallet">>,
  Expect<HasKey<CompleteTx["sign"], "withWalletSafe">>,
  Expect<HasKey<CompleteTx["sign"], "withWalletProgram">>,
  Expect<HasKey<CompleteTx["sign"], "withWalletEffect">>,
  Expect<HasKey<CompleteTx["sign"], "withPrivateKey">>,
  Expect<HasKey<CompleteTx["sign"], "withPrivateKeySafe">>,
  Expect<HasKey<CompleteTx["sign"], "withPrivateKeyProgram">>,
  Expect<HasKey<CompleteTx["sign"], "withPrivateKeyEffect">>,
  Expect<HasKey<CompleteTx["sign"], "withExternalSigner">>,
  Expect<HasKey<CompleteTx["sign"], "withExternalSignerSafe">>,
  Expect<HasKey<CompleteTx["sign"], "withExternalSignerProgram">>,
  Expect<HasKey<CompleteTx["sign"], "withExternalSignerEffect">>,
  Expect<HasKey<CompleteTx["sign"], "withWitness">>,
  Expect<HasKey<CompleteTx["sign"], "withWitnesses">>,
  Expect<HasKey<TxPartialSignBuilder, "partial">>,
  Expect<HasKey<TxPartialSignBuilder, "partialSafe">>,
  Expect<HasKey<TxPartialSignBuilder, "partialProgram">>,
  Expect<HasKey<TxPartialSignBuilder, "complete">>,
  Expect<HasKey<TxPartialSignBuilder, "completeSafe">>,
  Expect<HasKey<TxPartialSignBuilder, "completeProgram">>,
];

type _PartiallySignedTxSupported = [
  Expect<HasKey<PartiallySignedTx, "tx">>,
  Expect<HasKey<PartiallySignedTx, "txCbor">>,
  Expect<HasKey<PartiallySignedTx, "txHex">>,
  Expect<HasKey<PartiallySignedTx, "txId">>,
  Expect<HasKey<PartiallySignedTx, "txIdHex">>,
  Expect<HasKey<PartiallySignedTx, "metadata">>,
  Expect<HasKey<PartiallySignedTx, "toCBOR">>,
  Expect<HasKey<PartiallySignedTx, "toJSON">>,
  Expect<HasKey<PartiallySignedTx, "toHash">>,
  Expect<HasKey<PartiallySignedTx, "producedOutputs">>,
  Expect<HasKey<PartiallySignedTx, "producedOutput">>,
  Expect<HasKey<PartiallySignedTx, "assemble">>,
  Expect<HasKey<PartiallySignedTx, "toPartialWitnessBundle">>,
  Expect<HasKey<PartiallySignedTx, "toPartialWitnessBundleCbor">>,
];

type _SubmittedTxSupported = [
  Expect<HasKey<SubmittedTx, "producedOutputs">>,
  Expect<HasKey<SubmittedTx, "toCBOR">>,
  Expect<HasKey<SubmittedTx, "toJSON">>,
  Expect<HasKey<SubmittedTx, "toHash">>,
  Expect<HasKey<SubmittedTx, "producedOutput">>,
  Expect<HasKey<SubmittedTx, "statusSafe">>,
  Expect<HasKey<SubmittedTx, "statusProgram">>,
  Expect<HasKey<SubmittedTx, "awaitStatusSafe">>,
  Expect<HasKey<SubmittedTx, "awaitStatusProgram">>,
];

type _SelectWalletSupported = [
  Expect<HasKey<LucidMidgard["selectWallet"], "fromSeed">>,
  Expect<HasKey<LucidMidgard["selectWallet"], "fromPrivateKey">>,
  Expect<HasKey<LucidMidgard["selectWallet"], "fromExternalSigner">>,
  Expect<HasKey<LucidMidgard["selectWallet"], "fromAddress">>,
];

type _AttachSupported = [
  Expect<HasKey<TxBuilder["attach"], "Script">>,
  Expect<HasKey<TxBuilder["attach"], "NativeScript">>,
  Expect<HasKey<TxBuilder["attach"], "SpendingValidator">>,
  Expect<HasKey<TxBuilder["attach"], "MintingPolicy">>,
  Expect<HasKey<TxBuilder["attach"], "ObserverValidator">>,
  Expect<HasKey<TxBuilder["attach"], "ReferenceScriptMetadata">>,
  Expect<HasKey<TxBuilder["attach"], "Datum">>,
];

const _observerValidator: ObserverValidator = {
  language: "PlutusV3",
  script: "00",
};
const _midgardObserverValidator: ObserverValidator = {
  language: "MidgardV1",
  script: "00",
};
const _spendingValidator: SpendingValidator = _observerValidator;
const _mintingPolicy: MintingPolicy = _midgardObserverValidator;
const _noPlutusV2Observer: ObserverValidator = {
  // @ts-expect-error Midgard does not support PlutusV2 observer validators.
  language: "PlutusV2",
  script: "00",
};
const _trustedReferenceMetadata: TrustedReferenceScriptMetadata = {
  txHash: "00".repeat(32),
  outputIndex: 0,
  language: "MidgardV1",
  scriptHash: "11".repeat(28),
  scriptCborHash: "22".repeat(32),
};
const _readFromOptions: ReadFromOptions = {
  trustedReferenceScripts: [_trustedReferenceMetadata],
};

type _ProviderSupported = [
  Expect<HasKey<MidgardProvider, "getUtxos">>,
  Expect<HasKey<MidgardProvider, "getUtxoByOutRef">>,
  Expect<HasKey<MidgardProvider, "getProtocolInfo">>,
  Expect<HasKey<MidgardProvider, "getProtocolParameters">>,
  Expect<HasKey<MidgardProvider, "getCurrentSlot">>,
  Expect<HasKey<MidgardProvider, "submitTx">>,
  Expect<HasKey<MidgardProvider, "getTxStatus">>,
  Expect<HasKey<MidgardProvider, "diagnostics">>,
];

declare const midgard: LucidMidgard;
declare const builder: TxBuilder;
declare const nodeProvider: MidgardNodeProvider;

// @ts-expect-error Midgard does not support Cardano delegation queries.
type _NoDelegationAt = LucidMidgard["delegationAt"];
// @ts-expect-error Midgard does not support Cardano metadata queries.
type _NoMetadataOf = LucidMidgard["metadataOf"];
// @ts-expect-error Midgard has no slot/time conversion model yet.
type _NoUnixTimeToSlot = LucidMidgard["unixTimeToSlot"];
// @ts-expect-error Cardano browser wallet APIs do not prove Midgard body-hash signing.
type _NoSelectWalletFromAPI = LucidMidgard["selectWallet"]["fromAPI"];
// @ts-expect-error Midgard does not support Cardano staking.
type _NoRegisterStake = TxBuilder["registerStake"];
// @ts-expect-error Midgard does not support Cardano staking.
type _NoDeRegisterStake = TxBuilder["deRegisterStake"];
// @ts-expect-error Midgard does not support Cardano withdrawals.
type _NoWithdraw = TxBuilder["withdraw"];
// @ts-expect-error Midgard does not support Cardano certificate registration.
type _NoRegister = TxBuilder["register"];
// @ts-expect-error Midgard does not support Cardano certificate deregistration.
type _NoDeregister = TxBuilder["deregister"];
// @ts-expect-error Midgard does not support Cardano delegation.
type _NoDelegateTo = TxBuilder["delegateTo"];
// @ts-expect-error Midgard does not support Cardano delegation.
type _NoDelegate = TxBuilder["delegate"];
// @ts-expect-error Midgard does not support Cardano stake registration/delegation.
type _NoRegisterAndDelegate = TxBuilder["registerAndDelegate"];
// @ts-expect-error Midgard does not support Cardano governance.
type _NoUpdateDRep = TxBuilder["updateDRep"];
// @ts-expect-error Midgard does not support Cardano committee certificates.
type _NoAuthCommitteeHot = TxBuilder["authCommitteeHot"];
// @ts-expect-error Midgard does not support Cardano committee certificates.
type _NoResignCommitteeHot = TxBuilder["resignCommitteeHot"];
// @ts-expect-error Midgard does not support Cardano metadata attachment.
type _NoAttachMetadata = TxBuilder["attachMetadata"];
// @ts-expect-error Midgard has no certificate validators.
type _NoAttachCertificateValidator = TxBuilder["attach"]["CertificateValidator"];
// @ts-expect-error Midgard observers are not Cardano withdrawals.
type _NoAttachWithdrawalValidator = TxBuilder["attach"]["WithdrawalValidator"];
// @ts-expect-error Midgard has no vote validators.
type _NoAttachVoteValidator = TxBuilder["attach"]["VoteValidator"];
// @ts-expect-error Midgard has no proposal validators.
type _NoAttachProposeValidator = TxBuilder["attach"]["ProposeValidator"];
// @ts-expect-error Midgard must not expose Lucid CML builder config.
type _NoLucidConfig = TxBuilder["lucidConfig"];
// @ts-expect-error Lucid Effect internals are not a Midgard builder API.
type _NoGetPrograms = TxBuilder["getPrograms"];

describe("api parity type guards", () => {
  it("keeps runtime surface centered on Midgard objects", () => {
    expect(typeof midgard).toBe("undefined");
    expect(typeof builder).toBe("undefined");
    expect(typeof nodeProvider).toBe("undefined");
  });
});
