import {
  type CborItem as Item,
  decodeCborItem,
  renderCborItem as generic,
} from "../cbor/item.js";

/**
 * Renders the node's LocalTxSubmission rejection as one readable line.
 *
 * The bytes are the hard-fork combinator's `ApplyTxErr`: `[[era, failures]]`,
 * or `[txEra, ledgerEra]` for an era mismatch. Conway failures are a list of
 * LEDGER predicate failures, each a `[tag, ...fields]` sum that nests
 * UTXOW → UTXO → UTXOS (or CERTS → CERT → DELEG/POOL/GOVCERT, or GOV).
 *
 * This is a diagnostic, never a decision: the raw bytes stay authoritative on
 * {@link L1SubmitRejectedError}. Each failure is named by its exact ledger
 * constructor (grep-able in cardano-ledger) and the common ones have their
 * fields labelled; any tag or shape this does not know is printed
 * structurally instead of guessed at, and bytes that do not decode at all
 * come back as `undecodable`. It never throws.
 *
 * Tags follow the `EncCBOR` instances of cardano-ledger-conway 1.22.1.0, the
 * ledger of cardano-node 11.1.0 (eras/conway/impl/src/Cardano/Ledger/Conway/
 * Rules/{Ledger,Utxow,Utxo,Utxos,Certs,Cert,Deleg,GovCert,Gov}.hs, Shelley
 * Rules/Pool.hs, Alonzo Rules/Utxos.hs); they are unchanged through 1.24.0.0.
 */
export const describeLedgerRejection = (rejection: Uint8Array): string => {
  let item: Item;
  try {
    const decoded = decodeCborItem(rejection, 0);
    if (decoded.next !== rejection.length)
      return `undecodable (${rejection.length - decoded.next} trailing bytes)`;
    item = decoded.item;
  } catch (error) {
    return `undecodable (${error instanceof Error ? error.message : String(error)})`;
  }
  return clip(renderEnvelope(item));
};

const MAX_LINE = 4_096;
const CONWAY_ERA = 6n;
const ERA_NAMES = [
  "Byron",
  "Shelley",
  "Allegra",
  "Mary",
  "Alonzo",
  "Babbage",
  "Conway",
  "Dijkstra",
];

const renderEnvelope = (item: Item): string => {
  const outer = arrayOf(item);
  const era = outer?.length === 1 ? arrayOf(outer[0]) : undefined;
  if (era?.length === 2 && era[0]?.kind === "int") {
    const failures = arrayOf(era[1]);
    if (era[0].value === CONWAY_ERA && failures !== undefined)
      return failures.map((failure) => renderSum(LEDGER, failure)).join("; ");
    const name = ERA_NAMES[Number(era[0].value)] ?? `era ${era[0].value}`;
    return `${name} era: ${generic(era[1] as Item)}`;
  }
  if (outer?.length === 2)
    return `era mismatch (transaction, ledger): ${generic(item)}`;
  return generic(item);
};

/** One constructor of a ledger predicate-failure sum. */
type Arm = Readonly<{
  name: string;
  /** A wrapper arm: its single field is a failure of this rule. */
  inner?: () => Rules;
  /** Labels the fields after the tag; absent renders them structurally. */
  fields?: (fields: readonly Item[]) => string;
}>;
type Rules = Readonly<Record<number, Arm>>;

const renderSum = (rules: Rules, item: Item | undefined): string => {
  const parts = arrayOf(item);
  const tag = parts?.[0];
  const arm =
    tag?.kind === "int" && tag.value <= 64n
      ? rules[Number(tag.value)]
      : undefined;
  if (parts === undefined || arm === undefined)
    return `unknown failure ${item === undefined ? "(none)" : generic(item)}`;
  const fields = parts.slice(1);
  if (arm.inner !== undefined && fields.length === 1)
    return renderSum(arm.inner(), fields[0]);
  if (fields.length === 0) return arm.name;
  try {
    return `${arm.name} ${arm.fields?.(fields) ?? fields.map(generic).join(" ")}`;
  } catch {
    return `${arm.name} ${fields.map(generic).join(" ")}`;
  }
};

const clip = (line: string): string =>
  line.length > MAX_LINE
    ? `${line.slice(0, MAX_LINE)}…(+${line.length - MAX_LINE} chars)`
    : line;

const arrayOf = (item: Item | undefined): readonly Item[] | undefined =>
  item?.kind === "array" ? item.items : undefined;

const expectArray = (item: Item | undefined): readonly Item[] => {
  const items = arrayOf(item);
  if (items === undefined) throw new Error("expected an array");
  return items;
};

const int = (item: Item | undefined): bigint => {
  if (item?.kind !== "int") throw new Error("expected an integer");
  return item.value;
};

const hexOf = (item: Item | undefined): string => {
  if (item?.kind !== "bytes") throw new Error("expected bytes");
  return item.value.toString("hex");
};

/** A `StrictMaybe`: `[]` or `[x]`. */
const maybe = (item: Item | undefined, render: (x: Item) => string): string => {
  const items = expectArray(item);
  return items.length === 0 ? "none" : render(items[0] as Item);
};

/** `[txId, index]`, as `txId#index`. */
const txIn = (item: Item): string => {
  const [id, index] = expectArray(item);
  return `${hexOf(id)}#${int(index)}`;
};

const listOf = (item: Item | undefined, render: (x: Item) => string): string =>
  `{${expectArray(item).map(render).join(", ")}}`;

const PURPOSES = ["spend", "mint", "cert", "reward", "vote", "propose"];

/** A `PlutusPurpose AsIx`: `[purpose, index]`, as `spend#0`. */
const redeemerPointer = (item: Item): string => {
  const [purpose, index] = expectArray(item);
  return `${PURPOSES[Number(int(purpose))] ?? `purpose ${int(purpose)}`}#${int(index)}`;
};

/** A Mary value: `coin` or `[coin, {policy: {name: quantity}}]`, per asset. */
const assets = (item: Item): Map<string, bigint> => {
  const amounts = new Map<string, bigint>();
  const parts = item.kind === "int" ? [item] : expectArray(item);
  amounts.set("lovelace", int(parts[0]));
  const multiAsset = parts[1];
  if (multiAsset?.kind === "map")
    for (const [policy, names] of multiAsset.entries) {
      if (names.kind !== "map") throw new Error("expected an asset map");
      for (const [name, quantity] of names.entries)
        amounts.set(`${hexOf(policy)}.${assetName(name)}`, int(quantity));
    }
  return amounts;
};

const assetName = (item: Item): string => {
  if (item?.kind !== "bytes") throw new Error("expected bytes");
  return /^[\x21-\x7e]+$/u.test(item.value.toString("latin1"))
    ? item.value.toString("latin1")
    : item.value.toString("hex");
};

const valueText = (amounts: ReadonlyMap<string, bigint>): string =>
  [...amounts]
    .map(([unit, quantity]) =>
      unit === "lovelace" ? `${quantity} lovelace` : `${quantity} ${unit}`,
    )
    .join(" + ");

/** `consumed C, produced P (produced - consumed: ...)`. */
const valueNotConserved = ([consumed, produced]: readonly Item[]): string => {
  const ins = assets(consumed as Item);
  const outs = assets(produced as Item);
  const delta: string[] = [];
  for (const unit of new Set([...ins.keys(), ...outs.keys()])) {
    const difference = (outs.get(unit) ?? 0n) - (ins.get(unit) ?? 0n);
    if (difference !== 0n)
      delta.push(`${difference > 0n ? "+" : ""}${difference} ${unit}`);
  }
  return `consumed ${valueText(ins)}, produced ${valueText(outs)} (produced - consumed: ${delta.join(", ") || "0"})`;
};

const outsideValidityInterval = ([interval, slot]: readonly Item[]): string => {
  const [before, hereafter] = expectArray(interval);
  const current = int(slot);
  const lower = maybe(before, (x) => int(x).toString());
  const upper = maybe(hereafter, (x) => int(x).toString());
  const notes: string[] = [];
  if (lower !== "none" && current < BigInt(lower))
    notes.push(`lower bound ${BigInt(lower) - current} slots ahead`);
  if (upper !== "none" && current >= BigInt(upper))
    notes.push(`upper bound passed ${current - BigInt(upper) + 1n} slots ago`);
  return `validity [${lower}, ${upper}) at ledger slot ${current}${notes.length > 0 ? ` (${notes.join(", ")})` : ""}`;
};

/** `Mismatch` fields as `labelA x, labelB y`, in encoded order. */
const pair =
  (first: string, second: string, render: (x: Item) => string = generic) =>
  ([a, b]: readonly Item[]): string =>
    `${first} ${render(a as Item)}, ${second} ${render(b as Item)}`;

const hashes = ([set]: readonly Item[]): string => listOf(set, (x) => hexOf(x));
const inputs = ([set]: readonly Item[]): string => listOf(set, txIn);

const FAILURE_DESCRIPTION: Rules = {
  1: {
    name: "PlutusFailure",
    fields: ([message]) => generic(message as Item),
  },
};

const TAG_MISMATCH: Rules = {
  0: { name: "PassedUnexpectedly" },
  1: {
    name: "FailedUnexpectedly",
    fields: ([failures]) =>
      expectArray(failures)
        .map((failure) => renderSum(FAILURE_DESCRIPTION, failure))
        .join(", "),
  },
};

const UTXOS: Rules = {
  0: {
    name: "ValidationTagMismatch",
    fields: ([isValid, description]) =>
      `isValid ${generic(isValid as Item)}: ${renderSum(TAG_MISMATCH, description)}`,
  },
  1: { name: "CollectErrors" },
};

const UTXO: Rules = {
  0: { name: "UtxosFailure", inner: () => UTXOS },
  1: { name: "BadInputsUTxO", fields: inputs },
  2: { name: "OutsideValidityIntervalUTxO", fields: outsideValidityInterval },
  3: { name: "MaxTxSizeUTxO", fields: pair("size", "max") },
  4: { name: "InputSetEmptyUTxO" },
  5: { name: "FeeTooSmallUTxO", fields: pair("required", "supplied") },
  6: { name: "ValueNotConservedUTxO", fields: valueNotConserved },
  7: { name: "WrongNetwork" },
  8: { name: "WrongNetworkWithdrawal" },
  9: { name: "OutputTooSmallUTxO" },
  10: { name: "OutputBootAddrAttrsTooBig" },
  11: { name: "OutputTooBigUTxO" },
  12: { name: "InsufficientCollateral", fields: pair("balance", "required") },
  13: { name: "ScriptsNotPaidUTxO" },
  14: { name: "ExUnitsTooBigUTxO", fields: pair("max", "supplied") },
  15: { name: "CollateralContainsNonADA" },
  16: { name: "WrongNetworkInTxBody", fields: pair("expected", "supplied") },
  17: { name: "OutsideForecast" },
  18: { name: "TooManyCollateralInputs", fields: pair("max", "supplied") },
  19: { name: "NoCollateralInputs" },
  20: {
    name: "IncorrectTotalCollateralField",
    fields: pair("balance", "declared"),
  },
  21: { name: "BabbageOutputTooSmallUTxO" },
  22: { name: "BabbageNonDisjointRefInputs", fields: inputs },
};

const maybeHash = (x: Item): string => maybe(x, (h) => hexOf(h));

const UTXOW: Rules = {
  0: { name: "UtxoFailure", inner: () => UTXO },
  1: { name: "InvalidWitnessesUTXOW" },
  2: { name: "MissingVKeyWitnessesUTXOW", fields: hashes },
  3: { name: "MissingScriptWitnessesUTXOW", fields: hashes },
  4: { name: "ScriptWitnessNotValidatingUTXOW", fields: hashes },
  5: { name: "MissingTxBodyMetadataHash" },
  6: { name: "MissingTxMetadata" },
  7: { name: "ConflictingMetadataHash", fields: pair("supplied", "expected") },
  8: { name: "InvalidMetadata" },
  9: { name: "ExtraneousScriptWitnessesUTXOW", fields: hashes },
  10: { name: "MissingRedeemers" },
  11: {
    name: "MissingRequiredDatums",
    fields: pair("missing", "received", (x) => listOf(x, (h) => hexOf(h))),
  },
  12: {
    name: "NotAllowedSupplementalDatums",
    fields: pair("unallowed", "acceptable", (x) => listOf(x, (h) => hexOf(h))),
  },
  13: {
    name: "PPViewHashesDontMatch",
    fields: pair("supplied", "expected", maybeHash),
  },
  14: { name: "UnspendableUTxONoDatumHash", fields: inputs },
  15: {
    name: "ExtraRedeemers",
    fields: ([pointers]) => listOf(pointers, redeemerPointer),
  },
  16: { name: "MalformedScriptWitnesses", fields: hashes },
  17: { name: "MalformedReferenceScripts", fields: hashes },
  18: {
    name: "ScriptIntegrityHashMismatch",
    fields: ([mismatch]) =>
      pair("supplied", "expected", maybeHash)(expectArray(mismatch)),
  },
};

const DELEG: Rules = {
  1: { name: "IncorrectDepositDELEG" },
  2: { name: "StakeKeyRegisteredDELEG" },
  3: { name: "StakeKeyNotRegisteredDELEG" },
  4: { name: "StakeKeyHasNonZeroAccountBalanceDELEG" },
  5: { name: "DelegateeDRepNotRegisteredDELEG" },
  6: { name: "DelegateeStakePoolNotRegisteredDELEG" },
  7: { name: "DepositIncorrectDELEG" },
  8: { name: "RefundIncorrectDELEG" },
};

const POOL: Rules = {
  0: { name: "StakePoolNotRegisteredOnKeyPOOL" },
  1: { name: "StakePoolRetirementWrongEpochPOOL" },
  3: { name: "StakePoolCostTooLowPOOL", fields: pair("supplied", "expected") },
  4: { name: "WrongNetworkPOOL" },
  5: { name: "PoolMedataHashTooBig" },
  6: { name: "VRFKeyHashAlreadyRegistered" },
};

const GOVCERT: Rules = {
  0: { name: "ConwayDRepAlreadyRegistered" },
  1: { name: "ConwayDRepNotRegistered" },
  2: { name: "ConwayDRepIncorrectDeposit" },
  3: { name: "ConwayCommitteeHasPreviouslyResigned" },
  4: { name: "ConwayDRepIncorrectRefund" },
  5: { name: "ConwayCommitteeIsUnknown" },
};

const CERT: Rules = {
  1: { name: "DelegFailure", inner: () => DELEG },
  2: { name: "PoolFailure", inner: () => POOL },
  3: { name: "GovCertFailure", inner: () => GOVCERT },
};

const CERTS: Rules = {
  0: { name: "WithdrawalsNotInRewardsCERTS" },
  1: { name: "CertFailure", inner: () => CERT },
};

const GOV: Rules = Object.fromEntries(
  [
    "GovActionsDoNotExist",
    "MalformedProposal",
    "ProposalProcedureNetworkIdMismatch",
    "TreasuryWithdrawalsNetworkIdMismatch",
    "ProposalDepositIncorrect",
    "DisallowedVoters",
    "ConflictingCommitteeUpdate",
    "ExpirationEpochTooSmall",
    "InvalidPrevGovActionId",
    "VotingOnExpiredGovAction",
    "ProposalCantFollow",
    "InvalidGuardrailsScriptHash",
    "DisallowedProposalDuringBootstrap",
    "DisallowedVotesDuringBootstrap",
    "VotersDoNotExist",
    "ZeroTreasuryWithdrawals",
    "ProposalReturnAccountDoesNotExist",
    "TreasuryWithdrawalReturnAccountsDoNotExist",
    "UnelectedCommitteeVoters",
  ].map((name, tag) => [tag, { name }]),
);

const LEDGER: Rules = {
  1: { name: "ConwayUtxowFailure", inner: () => UTXOW },
  2: { name: "ConwayCertsFailure", inner: () => CERTS },
  3: { name: "ConwayGovFailure", inner: () => GOV },
  4: { name: "ConwayWdrlNotDelegatedToDRep", fields: hashes },
  5: {
    name: "ConwayTreasuryValueMismatch",
    fields: pair("expected", "supplied"),
  },
  6: { name: "ConwayTxRefScriptsSizeTooBig", fields: pair("size", "max") },
  7: { name: "ConwayMempoolFailure" },
  8: { name: "ConwayWithdrawalsMissingAccounts" },
  9: { name: "ConwayIncompleteWithdrawals" },
};
