# Pissfix 🚀

Pissfix is a blazingly fast 🚀 programming language that transpiles to a "intersting" and not well known programming language called "Postfix".
Postfix is a stack-based language that uses reverse Polish notation to express arithmetic expressions.
Pissfix allows you to write code in a more intuitive and readable way 💁‍♀️👌.

Pissfix is written in Rust 🦀, which is also blazing fast 🚀 and memory safe 🔒💾. \
The transpilation time on a medium sized program just takes a few microseconds 🚀🚀😱. \
Pissfix is an amazing language ✨ with tons of upsides, such as:

- Easy to learn and use 🧾🔠
- Compatible with any platform that supports Postfix ✅
- Supports functions, anonymous functions, variables, loops, and conditional statements 💭
- Has a rich set of built-in operators and functions 💰🅱
- Allows comments that carry over to postfix 🆗

## Example

Here is an example of how to write a function that computes the factorial of a given number in Pissfix:

```fs
fun factorial(n: Int) -> Int {
    product = 1
    while n != 0 {
        product *= n # This does the thing
        n -= 1
    }
    product
}
```

The transpiled Postfix code would look like this:

```
:factorial(n :Int -> :Int) {
    1 product!
    {
        n 0 !=
        not breakif
        product n
        * # This does the thing
        product!
        n 1 -
        n!
    } loop
    product
} fun
```

As you can see, Pissfix code is much more readable and concise than Postfix code 😊.

## Usage

To transpile a Pissfix file to a Postfix file, you can use the following command:

```bash
cargo run --release -- input.piss -o out.pf
```

## Contributing

Pissfix is an open source project and welcomes contributions from anyone who is interested. You can report issues, suggest features, or submit pull requests on the github repository.

## License

Pissfix is licensed under the GNU GPL 3 License.
