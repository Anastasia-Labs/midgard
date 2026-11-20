import Link from 'next/link';
import { ChevronRight, Layers, ShieldCheck, Server, Radio } from 'lucide-react';

export default function HomePage() {
  return (
    <main className="flex flex-col items-center min-h-screen pt-32 pb-24 px-6 relative bg-fd-background text-fd-foreground font-sans selection:bg-fd-primary/20">
      <div className="z-10 max-w-5xl w-full flex flex-col items-center">
        <span className="mb-6 inline-flex items-center rounded-full border border-fd-border px-3 py-1 text-xs font-medium text-fd-muted-foreground font-mono">
          Optimistic rollup · Cardano L2
        </span>

        <h1 className="font-display text-4xl sm:text-5xl md:text-6xl font-light tracking-tight text-fd-foreground mb-6 text-center leading-tight">
          <span className="font-semibold text-fd-primary">Midgard</span> Documentation
        </h1>

        <p className="text-lg md:text-xl text-fd-muted-foreground max-w-3xl mb-12 text-center leading-relaxed">
          Midgard is Cardano&apos;s first optimistic rollup protocol. These docs
          help you build native L2 transactions, run an operator node,
          understand data availability and fraud proofs, and inspect the
          on-chain validators behind the protocol.
        </p>

        <div className="flex flex-col sm:flex-row gap-4 mb-24 w-full sm:w-auto">
          <Link
            href="/docs"
            className="inline-flex items-center justify-center rounded-md bg-fd-primary text-fd-primary-foreground px-8 py-3 text-sm font-medium transition-all hover:bg-fd-primary/90 active:scale-[0.98]"
          >
            Read the docs
            <ChevronRight className="ml-1 h-4 w-4" />
          </Link>
          <a
            href="https://github.com/Anastasia-Labs/midgard"
            target="_blank"
            rel="noreferrer"
            className="inline-flex items-center justify-center rounded-md border border-fd-border bg-transparent px-8 py-3 text-sm font-medium text-fd-foreground transition-colors hover:border-fd-primary hover:bg-fd-accent"
          >
            View the repository
          </a>
        </div>

        <div className="mb-24 w-full max-w-2xl rounded-xl border border-fd-border bg-fd-card overflow-hidden">
          <div className="flex items-center gap-2 border-b border-fd-border px-4 py-2.5 font-mono text-xs text-fd-muted-foreground">
            <span className="h-2.5 w-2.5 rounded-full bg-fd-primary/70" />
            transfer.ts
          </div>
          <pre className="overflow-x-auto p-4 text-sm font-mono leading-relaxed text-fd-foreground">
            <code>{`import { LucidMidgard, MidgardNodeProvider } from "@al-ft/lucid-midgard";

const provider = await MidgardNodeProvider.create({
  endpoint: "http://127.0.0.1:3000",
});

const midgard = await LucidMidgard.new(provider, "Preprod");
midgard.selectWallet.fromSeed(seedPhrase);

const tx = await midgard
  .newTx()
  .pay.ToAddress(recipient, { lovelace: 5_000_000n })
  .complete();

const signed = await tx.sign.withWallet().complete();
const submitted = await signed.submit();
await submitted.awaitStatus({ until: "accepted" });`}</code>
          </pre>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-8 text-left w-full border-t border-fd-border pt-16">
          <Feature
            icon={<Layers className="h-5 w-5" />}
            title="Build native L2 transactions"
            href="/docs/sdk/lucid-midgard/quickstart"
            body="Build and sign Midgard L2 native transactions using lucid-midgard and the midgard-core codec."
          />
          <Feature
            icon={<Server className="h-5 w-5" />}
            title="Run an operator node"
            href="/docs/operators/node/overview"
            body="Serve the HTTP API, commit and merge blocks, process deposits and withdrawals, and run the background fibers."
          />
          <Feature
            icon={<Radio className="h-5 w-5" />}
            title="Verify data availability"
            href="/docs/watchers/da-committee-node"
            body="The libp2p data-availability committee: attestation, payload exchange, and the on-chain reconciliation that lets watchers verify availability and challenge invalid commitments."
          />
          <Feature
            icon={<ShieldCheck className="h-5 w-5" />}
            title="Understand fraud proofs"
            href="/docs/onchain/overview"
            body="The optimistic security model: fraud-proof state machines and the Aiken on-chain validators that resolve challenges."
          />
        </div>
      </div>
    </main>
  );
}

function Feature({
  icon,
  title,
  body,
  href,
}: {
  icon: React.ReactNode;
  title: string;
  body: string;
  href: string;
}) {
  return (
    <Link
      href={href}
      className="group flex flex-col items-start text-fd-muted-foreground transition-colors hover:text-fd-foreground"
    >
      <span className="mb-4 text-fd-muted-foreground group-hover:text-fd-primary transition-colors">
        {icon}
      </span>
      <h3 className="text-base font-medium text-fd-foreground mb-2 font-display">
        {title}
      </h3>
      <p className="text-sm leading-relaxed">{body}</p>
    </Link>
  );
}
