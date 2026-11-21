import { expect, it } from "vitest";

import { type RunEnv, servicePorts } from "../src/devnet-stack/layout.js";

const run: RunEnv = {
  runId: "synthetic",
  composeProject: "synthetic",
  networkMagic: 42,
  ogmiosPort: 2337,
  kupoPort: 2442,
  postgresPort: 5432,
  postgresUser: "synthetic",
  postgresPassword: "synthetic",
  postgresDatabase: "synthetic",
  cardanoImage: "synthetic",
  postgresImage: "synthetic",
  portOffset: 0,
};

it.each([-4000, 30000, 0.5])(
  "refuses invalid declared port offset %s",
  (portOffset) => {
    expect(() => servicePorts({ ...run, portOffset })).toThrow(
      "valid TCP ports",
    );
  },
);

it("refuses a public-reader binding that collides with a recorded run service", () => {
  expect(() => servicePorts({ ...run, kupoPort: 7406 })).toThrow(
    "must not overlap",
  );
});
