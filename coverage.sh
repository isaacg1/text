set -v
rm target/debug/text-*
cargo test
kcov --verify --exclude-pattern=/build target/cov `find  | grep '^\./target/debug/text-[0-9a-f]*$'`
