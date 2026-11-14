/**
 * The runtime funding bound's ledger accounting outside inputs and outputs:
 * what a body mints or burns, and the stake and governance accounting a
 * funding transaction never changes.
 */
import type { CML } from "@lucid-evolution/lucid";

/** Adds the body's minted (and burned) quantities to the inputs' side. */
export const addMintedAssets = (
  inputAssets: Map<string, bigint>,
  mint: CML.Mint | undefined,
): void => {
  if (mint === undefined) return;
  const policies = mint.keys();
  for (let i = 0; i < policies.len(); i += 1) {
    const policyHash = policies.get(i);
    const assets = mint.get_assets(policyHash)!;
    const names = assets.keys();
    for (let j = 0; j < names.len(); j += 1) {
      const name = names.get(j),
        unit = policyHash.to_hex() + name.to_hex();
      inputAssets.set(unit, (inputAssets.get(unit) ?? 0n) + assets.get(name)!);
    }
  }
};

/** Withdraw-zero yielding never authorizes stake withdrawals or deposits. */
export const assertNoUnrelatedLedgerAccounting = (
  body: CML.TransactionBody,
): void => {
  const withdrawals = body.withdrawals();
  if (withdrawals !== undefined) {
    const keys = withdrawals.keys();
    for (let index = 0; index < keys.len(); index += 1)
      if (withdrawals.get(keys.get(index)) !== 0n)
        throw new Error("funding transaction includes a nonzero withdrawal");
  }
  if ((body.certs()?.len() ?? 0) !== 0 || body.donation() !== undefined)
    throw new Error("funding transaction changes unrelated ledger accounting");
};
