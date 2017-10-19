# How to set up CI 

## Getting docker

  * Get [docker](https://docs.docker.com/engine/installation/) go to your OS 
  version and install the fun and get a [docker id](https://cloud.docker.com/).
  * If `$ docker` gives you something we are good

## Getting gitlab-runner

Install gitlab runner as a background process:  
 * [linux](https://docs.gitlab.com/runner/install/linux-manually.html)  
 * [macOS](https://docs.gitlab.com/runner/install/osx.html)

Register the runner: `$ gitlab-runner register`  
(**gives hint on what to enter at each step of [this page](https://docs.gitlab.com/runner/register/index.html)**): 
 2. https://gitlab.doc.ic.ac.uk/
 3. j8GsNfxPCgkXzszkRsBV
 4. "Kyle's-XPS-runner"
 5. nothing(press enter don't write nothing you idiot)
 6. false
 7. false
 8. docker
 9. waskell-env (if you actually named your commit image waskell-env)

 That's all folks, try rebooting, pushing something to repo and see in 
 [gitlab](gitlab.doc.ic.ac.uk/waskell/compiler) in **pipeline** if build runs
 on your machine 

## For later, do not do that

>
Now let's create the container:  
```
$ docker pull fpco/stack-build
$ docker run -i -t fpco/stack-build /bin/bash
$ cd homes/
$ stack setup
$ stack new op tasty-travis
$ cd op
$ stack build
$ stack test
$ cd ..
$ rm -rf op
$ cabal update
$ cabal install alex
$ cabal install happy
$ exit
```

Now as you were in the container you must have seen the prompt being like 
`root@3b2cw92mfd: `. Copy the hash and :  
`$ docker commit -a "<author's name>" -m "Created waskell env" <your hash 3b2c...> waskell-env`  
>
