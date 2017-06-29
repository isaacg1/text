set -v
rm target/debug/text-*
cargo test
kcov --verify --exclude-pattern=/build target/cov target/debug/text-422a6011484e438a
