import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import type { Provider } from "@lucid-evolution/lucid";

/** The provider library honors its request timeout, but exposes no external
 * AbortSignal. Fence results and bound each fresh request by the same remainder.
 * A generation abort can leave that request alive until its existing timeout;
 * this adapter therefore does not establish immediate resource cancellation. */
export const watcherAvailabilityAttemptProvider = (
  scope: DaAvailabilityReadScope,
  configuredTimeoutMs: number,
  create: (timeoutMs: number) => Provider,
): Provider => {
  const read = <T>(run: (provider: Provider) => Promise<T>): Promise<T> =>
    scope.read(() => {
      scope.assertCurrent();
      return run(
        create(
          Math.max(
            1,
            Math.min(configuredTimeoutMs, Math.ceil(scope.remainingMs())),
          ),
        ),
      );
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
