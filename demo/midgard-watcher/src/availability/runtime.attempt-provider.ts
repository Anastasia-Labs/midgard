import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import type { Provider } from "@lucid-evolution/lucid";

/** The follower provider exposes no external AbortSignal: every request runs
 * inside the attempt's scope, which fences its result and bounds it by the
 * scope's remainder. A generation abort can leave a node query alive until it
 * answers; this adapter does not establish immediate resource cancellation. */
export const watcherAvailabilityAttemptProvider = (
  scope: DaAvailabilityReadScope,
  provider: Provider,
): Provider => {
  const read = <T>(run: (provider: Provider) => Promise<T>): Promise<T> =>
    scope.read(() => {
      scope.assertCurrent();
      return run(provider);
    });
  return {
    getProtocolParameters: () => read((p) => p.getProtocolParameters()),
    getUtxos: (address) => read((p) => p.getUtxos(address)),
    getUtxosWithUnit: (address, unit) =>
      read((p) => p.getUtxosWithUnit(address, unit)),
    getUtxoByUnit: (unit) => read((p) => p.getUtxoByUnit(unit)),
    getUtxosByOutRef: (refs) => read((p) => p.getUtxosByOutRef(refs)),
    getDelegation: (address) => read((p) => p.getDelegation(address)),
    getRewardAccount: (address) =>
      read((p) => {
        if (p.getRewardAccount === undefined)
          throw new Error(
            "Attempt provider omitted native reward-account authority",
          );
        return p.getRewardAccount(address);
      }),
    getDatum: (hash) => read((p) => p.getDatum(hash)),
    evaluateTx: (tx, refs) => read((p) => p.evaluateTx(tx, refs)),
    // The executor submits the persisted intent through its independent port.
    submitTx: () =>
      Promise.reject(new Error("Unsigned attempt provider cannot submit")),
    awaitTx: () =>
      Promise.reject(new Error("Signed inclusion requires its recovery scope")),
  };
};
