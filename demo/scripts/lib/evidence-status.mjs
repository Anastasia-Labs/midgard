// Shared status classification for the canonical-V1 evidence verifiers.
//
// Published statuses are routinely decorated with the reason they hold:
// `PASS (structural N/A)`, `PASS (LOCAL_PASS; Q57/QG3 owns LIVE)`,
// `PASS (live-verified on preprod)`. Comparing a whole status cell against the
// literal "PASS" therefore silently excludes exactly the rows whose decoration
// records how the claim was established, so every status-keyed rule written
// that way no-ops on them — finding V-2/V-12 of issue #519.
//
// `statusRole` reduces a status to its leading role token and `isPassStatus` is
// the base-role PASS test. Both were introduced inside
// Keep status parsing shared between the remaining domain reconciliation
// checks instead of growing subtly different conventions in each script.

export const statusRole = (status) =>
  (status ?? "")
    .replace(/[`*]/g, "")
    .trim()
    .split(/[\s(;,]/)[0]
    .toUpperCase();

export const isPassStatus = (status) => statusRole(status) === "PASS";
