#![allow(dead_code)]
#![allow(non_snake_case)]

use rand::prelude::*;

static CONFUSION: [u8; 512] = [
    0xac,0xd1,0x25,0x94,0x1f,0xb3,0x33,0x28,0x7c,0x2b,0x17,0xbc,0xf6,0xb0,0x55,0x5d,
    0x8f,0xd2,0x48,0xd4,0xd3,0x78,0x62,0x1a,0x02,0xf2,0x01,0xc9,0xaa,0xf0,0x83,0x71,
    0x72,0x4b,0x6a,0xe8,0xe9,0x42,0xc0,0x53,0x63,0x66,0x13,0x4a,0xc1,0x85,0xcf,0x0c,
    0x24,0x76,0xa5,0x6e,0xd7,0xa1,0xec,0xc6,0x04,0xc2,0xa2,0x5c,0x81,0x92,0x6c,0xda,
    0xc6,0x86,0xba,0x4d,0x39,0xa0,0x0e,0x8c,0x8a,0xd0,0xfe,0x59,0x96,0x49,0xe6,0xea,
    0x69,0x30,0x52,0x1c,0xe0,0xb2,0x05,0x9b,0x10,0x03,0xa8,0x64,0x51,0x97,0x02,0x09,
    0x8e,0xad,0xf7,0x36,0x47,0xab,0xce,0x7f,0x56,0xca,0x00,0xe3,0xed,0xf1,0x38,0xd8,
    0x26,0x1c,0xdc,0x35,0x91,0x43,0x2c,0x74,0xb4,0x61,0x9d,0x5e,0xe9,0x4c,0xbf,0x77,
    0x16,0x1e,0x21,0x1d,0x2d,0xa9,0x95,0xb8,0xc3,0x8d,0xf8,0xdb,0x34,0xe1,0x84,0xd6,
    0x0b,0x23,0x4e,0xff,0x3c,0x54,0xa7,0x78,0xa4,0x89,0x33,0x6d,0xfb,0x79,0x27,0xc4,
    0xf9,0x40,0x41,0xdf,0xc5,0x82,0x93,0xdd,0xa6,0xef,0xcd,0x8d,0xa3,0xae,0x7a,0xb6,
    0x2f,0xfd,0xbd,0xe5,0x98,0x66,0xf3,0x4f,0x57,0x88,0x90,0x9c,0x0a,0x50,0xe7,0x15,
    0x7b,0x58,0xbc,0x07,0x68,0x3a,0x5f,0xee,0x32,0x9f,0xeb,0xcc,0x18,0x8b,0xe2,0x57,
    0xb7,0x49,0x37,0xde,0xf5,0x99,0x67,0x5b,0x3b,0xbb,0x3d,0xb5,0x2d,0x19,0x2e,0x0d,
    0x93,0xfc,0x7e,0x06,0x08,0xbe,0x3f,0xd9,0x2a,0x70,0x9a,0xc8,0x7d,0xd8,0x46,0x65,
    0x22,0xf4,0xb9,0xa2,0x6f,0x12,0x1b,0x14,0x45,0xc7,0x87,0x31,0x60,0x29,0xf7,0x73,
    0x2c,0x97,0x72,0xcd,0x89,0xa6,0x88,0x4c,0xe8,0x83,0xeb,0x59,0xca,0x50,0x3f,0x27,
    0x4e,0xae,0x43,0xd5,0x6e,0xd0,0x99,0x7b,0x7c,0x40,0x0c,0x52,0x86,0xc1,0x46,0x12,
    0x5a,0x28,0xa8,0xbb,0xcb,0xf0,0x11,0x95,0x26,0x0d,0x34,0x66,0x22,0x18,0x6f,0x51,
    0x9b,0x3b,0xda,0xec,0x5e,0x00,0x2a,0xf5,0x8f,0x61,0xba,0x96,0xb3,0xd1,0x30,0xdc,
    0x33,0x75,0xe9,0x6d,0xc8,0xa1,0x3a,0x3e,0x5f,0x9d,0xfd,0xa9,0x31,0x9f,0xaa,0x85,
    0x2f,0x92,0xaf,0x67,0x78,0xa5,0xab,0x03,0x21,0x4f,0xb9,0xad,0xfe,0xf3,0x42,0xfc,
    0x17,0xd7,0xee,0xa3,0xd8,0x80,0x14,0x2e,0xa0,0x47,0x55,0xc4,0xff,0xe5,0x13,0x3f,
    0x81,0xb6,0x7a,0x94,0xd0,0xb5,0x54,0xbf,0x91,0xa7,0x37,0xf1,0x6b,0xc9,0x1b,0xb1,
    0x3c,0xb6,0xd9,0x32,0x24,0x8d,0xf2,0x82,0xb4,0xf9,0xdb,0x7d,0x44,0xfb,0x1e,0xd4,
    0xea,0x5d,0x35,0x69,0x23,0x71,0x57,0x01,0x06,0xe4,0x55,0x9a,0xa4,0x58,0x56,0xc7,
    0x4a,0x8c,0x8a,0xd6,0x6a,0x49,0x70,0xc5,0x8e,0x0a,0x62,0xdc,0x29,0x4b,0x42,0x41,
    0xcb,0x2b,0xb7,0xce,0x08,0xa1,0x76,0x1d,0x1a,0xb8,0xe3,0xcc,0x7e,0x48,0x20,0xe6,
    0xf8,0x45,0x93,0xde,0xc3,0x63,0x0f,0xb0,0xac,0x5c,0xba,0xdf,0x07,0x77,0xe7,0x4e,
    0x1f,0x28,0x10,0x6c,0x59,0xd3,0xdd,0x2d,0x65,0x39,0xb2,0x74,0x84,0x3d,0xf4,0xbd,
    0xc7,0x79,0x60,0x0b,0x4d,0x33,0x36,0x25,0xbc,0xe0,0x09,0xcf,0x5b,0xe2,0x38,0x9e,
    0xc0,0xef,0xd2,0x16,0x05,0xbe,0x53,0xf7,0xc2,0xc6,0xa2,0x24,0x98,0x1c,0xad,0x04
];
    
static DIFFUSION: [u32;32]=[
    0xf26cb481,0x16a5dc92,0x3c5ba924,0x79b65248,0x2fc64b18,0x615acd29,0xc3b59a42,0x976b2584,
    0x6cf281b4,0xa51692dc,0x5b3c24a9,0xb6794852,0xc62f184b,0x5a6129cd,0xb5c3429a,0x6b978425,
    0xb481f26c,0xdc9216a5,0xa9243c5b,0x524879b6,0x4b182fc6,0xcd29615a,0x9a42c3b5,0x2584976b,
    0x81b46cf2,0x92dca516,0x24a95b3c,0x4852b679,0x184bc62f,0x29cd5a61,0x429ab5c3,0x84256b97
];
    
// this is what you are solving for
static INPUT: [u8; 32] = [
    0x66,0xd5,0x4e,0x28,0x5f,0xff,0x6b,0x53,0xac,0x3b,0x34,0x14,0xb5,0x3c,0xb2,0xc6,
    0xa4,0x85,0x1e,0x0d,0x86,0xc7,0x4f,0xba,0x75,0x5e,0xcb,0xc3,0x6e,0x48,0x79,0x8f
];
    
// Ok, lets have some fun using types to denote the stages of the conversion
// See the wikipedia here to help inform nomenclature:
// https://en.wikipedia.org/wiki/Substitution%E2%80%93permutation_network
// The task is to reverse a substitution-permutation network

// the path is Key -> (substitute) -> KeyS -> permutate -> Key
// and we repeat for n rounds

struct Msg {
    data: [u8;Msg::SIZE],
}
impl Msg {
    const SIZE: usize = 16;

    fn from_u8(data: [u8;16]) -> Msg {
        Msg { data }
    }
    fn from_str(s: &str) -> Msg {
        let mut data = [0;Msg::SIZE];
        for (i,b) in s.bytes().take(Msg::SIZE).enumerate() {
            data[i] = b;
        }
        Msg{ data }
    }
}

#[derive(Clone)]
struct PostPermutate {
    data: [u8;32],
}
impl PostPermutate {
    fn substitute(mut self, S: &[u8]) -> PostSubstitute {
        let mut data = self.data;
        for i in 0..32 {
            let i_S = data[i] as usize;
            data[i] = S[i_S];
        }
        PostSubstitute {data: self.data}
    }

    fn from_msg(msg: &Msg, a_validator: &Validator, b_validator: &Validator) -> PostPermutate {
        assert!(msg.data.len() == 16);
        let mut rng = rand::thread_rng();

        let mut tmp: [u8;32] = [0;32];
        for (i,x) in msg.data.iter().enumerate() {
            loop {
                let a: u8 = rng.gen();
                let b = x ^ a;

                if a_validator.contains(a) && b_validator.contains(b) {
                    tmp[2*i] = a;
                    tmp[2*i+1] = b;
                    break;
                }
            }
        }
        PostPermutate {data: tmp}
    }

    fn to_msg(&self, A: &[u8], B: &[u8]) -> Msg {
        let mut msg: [u8;16] = [0;16];
        for i in 0..16 {
            let i_a = self.data[2*i] as usize;
            let i_b = self.data[2*i + 1] as usize;
            msg[i] = A[i_a] ^ B[i_b];
        }
        Msg::from_u8(msg)
    }

    fn depermutate(self, P: &[u32]) -> PostSubstitute {
        // by xor'ing everything except the ith element that was xor'd
        // we are left with the ith element _before_ the permutation
        //let mut data: [u8;32] = [0;32];
        let mut data: [u8;32] = self.data.clone();
        for (i,p) in P.iter().enumerate() {
            // xor everything except the ith entry
            let mask = !(1 << i);
            assert!((p & !mask) != 0b0);
            for j in 0..32 {
                let is_jth_bit_set = ((p&mask) >> j) & 1;
                assert!(j != i || is_jth_bit_set == 0);
                data[i] ^= self.data[j] * (is_jth_bit_set as u8);
            }
        }

        PostSubstitute { data }
    }
}
impl std::fmt::Display for PostPermutate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for x in self.data.iter() {
            write!(f, "0x{:x},", x);
        }
        write!(f, "")
    }
}
impl std::fmt::Display for PostSubstitute {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for x in self.data.iter() {
            write!(f, "0x{:x},", x);
        }
        write!(f, "")
    }
}
impl std::fmt::Debug for PostPermutate {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for x in self.data.iter() {
            write!(f, "0x{:x},", x);
        }
        write!(f, "")
    }
}


#[derive(PartialEq,Eq)]
struct PostSubstitute {
    data: [u8;32],
}
impl PostSubstitute {
    fn permutate(self, P: &[u32]) -> PostPermutate {
        let mut out = [0;32];
        for (i,p) in P.iter().enumerate() {
            for j in 0..32 {
                // the bit pattern in d determines which we xor
                let is_jth_bit_set = (p >> j) & 1;
                out[i] ^= self.data[j] * (is_jth_bit_set as u8);
            }
        }
        PostPermutate { data: out }
    }

    // returns KeyPossibilities
    // `KeyPosibilities` is meant to be used as an iterator permutating over the different possible substitutions.
    fn desubstitute(self, validator: &Validator) -> KeyIter {
        KeyIter::new(self, validator)
    }
}

// TODO: maybe this should just be on Key???
// TODO What if the name hinted at it being a Possibility?
// All the stuff we need to iterate through the possibilities for a KesS
// It's needed because of duplicate values is the Substitution array
struct KeyIter<'a> {
    // we need to store `KeyS` so we can do reverse lookup for index
    k: PostSubstitute,

    // we use the validator to get counts and rev_lookup
    validator: &'a Validator,
    i_duplicate: Vec<u8>,
    duplicate: Vec<u8>,
    iter: usize,
}

impl<'a> KeyIter<'a> {

    fn new(k: PostSubstitute, validator: &Validator) -> KeyIter {
        assert!(k.data.len() == 32);

        let mut i_duplicate: Vec<u8> = Vec::new();
        let mut duplicate: Vec<u8> = Vec::new();

        println!("KeyIter::k; {}", k);
        for (i,x) in k.data.iter().enumerate() {
            let x = *x;
            let i_x = x as usize;

            assert!(validator.value_count[i_x] == 1 || validator.value_count[i_x] == 2);

            if validator.value_count[x as usize] == 2 {
                i_duplicate.push(i as u8);
                duplicate.push(x);
            }
        }

        assert!(duplicate.len() == i_duplicate.len());
        println!("KeyIter.k; {}", k);

        KeyIter {
            k,
            validator,
            i_duplicate,
            duplicate,
            iter: 0,
        }
    }

    // returns the key based on the permutation
    fn get_key(&self) -> PostPermutate {
        assert!(self.iter < (1<<self.duplicate.len()));
        // create a copy
        // the reverse lookup here could be cached... would it be faster? idk
        let mut data: [u8;32] = self.k.data.clone();
        for x in data.iter_mut() {
            *x = self.validator.get_index(*x,0);
        }

        // lets apply the changes based on `curr_iter`
        for k in 0..self.duplicate.len() {
            let x = self.duplicate[k];
            let i = self.i_duplicate[k] as usize;
            let i_entry = (self.iter >> k) & 0b1;
            data[i] = self.validator.get_index(x, i_entry);
        }

        PostPermutate { data }
    }
}


impl<'a> Iterator for KeyIter<'a> {
    type Item = PostPermutate;
    fn next(&mut self) -> Option<PostPermutate> {
        // check if we are done
        let end = 1 << self.i_duplicate.len();
        if self.iter >= end {
            return None;
        }

        let item = self.get_key();
        self.iter += 1;

        Some(item)
    }
}


fn main() {
    // This is just for nomenclature
    // `P` for permutation
    // `S` for substitution
    // Each is needed to permatate and substitute
    let P: &[u32] = &DIFFUSION[0..32];
    let S: &[u8] = &CONFUSION[0..256];

    // these are used to go to/from Msg
    let A: &[u8] = &CONFUSION[0..256];
    let B: &[u8] = &CONFUSION[256..512];

    // Validators are needed for certain steps of the `reverse` process.
    // After all, we don't want to reverse a key that could not exist.
    let a_validator = Validator::from_array(A);
    let b_validator = Validator::from_array(B);

    // So, now lets get to work!!

    // `input` is what we are solving for
    let mut input: Option<PostPermutate> = None;

    // `msg` is what we are reversing... we want a cipher that decrypts to this message
    let msg = Msg::from_str("Hire me!!!!!!!!");

    // `ROUNDS` is the number of times we want to have to transform the `input` to get `msg`
    const ROUNDS: usize = 256;
    while input.is_none() {
        // from_msg uses rng so we don't get repeats
        let post_permutate = PostPermutate::from_msg(&msg, &a_validator, &b_validator); 
        println!("k[0]: {}", post_permutate);
        // here we call into a recursive fn to try to get to level 0
        input = reverse(ROUNDS, post_permutate, P, S, &a_validator);
        // if the recursive call succeeded we found our answer
        if input.is_some() {
            break;
        }
    }
    
    // print the results
    if let Some(input) = input {
        println!("Input found: {}", input);
    }
}

fn reverse(
        cycles_remaining: usize,
        key: PostPermutate,
        P: &[u32],
        S: &[u8],
        validator: &Validator
    ) -> Option<PostPermutate> 
{
    println!("cycle_num:{}", cycles_remaining);
    // if we've done all the cycles
    if cycles_remaining == 0 {
        return Some(key);
    }

    // if we can't depermutate it's a failure
    let post_substitute = key.depermutate(&P);
    if validator.cannot_reverse_lookup(&post_substitute) {
        println!("cannot reverse lookup all values. going up a level");
        return None;
    }

    // TODO make sure this iterates on the correct thing
    let key_iter = post_substitute.desubstitute(&validator);
    //println!("num_duplicates: {}", &key_iter.duplicate.len());
    for (i,pre_sub) in key_iter.enumerate() {
        println!("{}.{}", cycles_remaining, i);
        let reversed = reverse(cycles_remaining - 1, pre_sub, P, S, validator);
        if reversed.is_some() {
            return reversed;
        }
    }
    return None;
}

// TODO: we only care about this when we reverse the permutation
struct Validator {
    value_count: [u8;256],
    value_indicies: [[u8;2];256],
}
impl Validator {
    fn from_array(A: &[u8]) -> Validator {
        assert!(A.len() == 256);

        let mut value_count: [u8;256] = [0;256];
        let mut value_indicies: [[u8;2];256] = [[0;2];256];
        
        for (i,x) in A.iter().enumerate() {
            // we have a maximum storage of two entries
            assert!(value_count[*x as usize] == 0
                 || value_count[*x as usize] == 1
            );
            value_indicies[*x as usize][value_count[*x as usize] as usize] = i as u8;
            value_count[*x as usize] += 1;
        }
        Validator{ value_count, value_indicies}
    }

    fn contains(&self, x: u8) -> bool {
        return self.value_count[x as usize] == 1
            || self.value_count[x as usize] == 2;
    }
    fn get_index(&self, x: u8, i: usize) -> u8 {
        self.value_indicies[x as usize][i as usize]
    }
    fn cannot_reverse_lookup(&self, post_substitute: &PostSubstitute) -> bool {
        post_substitute.data.iter().any(|&x| self.value_count[x as usize] == 0)
    }
    fn can_reverse_lookup(&self, post_substitute: &PostSubstitute) -> bool {
        post_substitute.data.iter().all(|&x| self.value_count[x as usize] > 0)
    }
}