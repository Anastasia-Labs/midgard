import { RootProvider } from 'fumadocs-ui/provider/next';
import './global.css';
import 'fumadocs-twoslash/twoslash.css';
import { Inter, Space_Grotesk, JetBrains_Mono } from 'next/font/google';
import type { Metadata } from 'next';

const inter = Inter({
  subsets: ['latin'],
  variable: '--font-inter',
});

const spaceGrotesk = Space_Grotesk({
  subsets: ['latin'],
  variable: '--font-space-grotesk',
});

const jetbrainsMono = JetBrains_Mono({
  subsets: ['latin'],
  variable: '--font-jetbrains-mono',
});

// Mirrors next.config.mjs. Raw fetches and metadata URLs are not rewritten by
// basePath, so they have to prepend it themselves.
const basePath = process.env.NEXT_PUBLIC_BASE_PATH ?? '';

export const metadata: Metadata = {
  metadataBase: new URL(
    process.env.NEXT_PUBLIC_SITE_URL ?? 'http://localhost:3000'
  ),
  title: {
    default: 'Midgard Documentation',
    template: '%s | Midgard',
  },
  description:
    "Documentation for Midgard, Cardano's first optimistic rollup protocol: SDK, node operators, watchers, fault proofs, on-chain validators, and protocol spec.",
  icons: { icon: `${basePath}/favicon.png` },
};

export default function Layout({ children }: { children: React.ReactNode }) {
  return (
    <html
      lang="en"
      suppressHydrationWarning
      className={`${inter.variable} ${spaceGrotesk.variable} ${jetbrainsMono.variable}`}
    >
      <body
        className="flex flex-col min-h-screen font-sans"
        suppressHydrationWarning
      >
        <RootProvider
          search={{
            options: { type: 'static', api: `${basePath}/api/search` },
          }}
        >
          {children}
        </RootProvider>
      </body>
    </html>
  );
}
