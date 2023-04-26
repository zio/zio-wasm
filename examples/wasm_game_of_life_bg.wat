(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (type (;1;) (func (param i32 i32)))
  (type (;2;) (func (param i32 i32 i32) (result i32)))
  (type (;3;) (func (param i32) (result i32)))
  (type (;4;) (func (param i32)))
  (type (;5;) (func (param i32 i32 i32)))
  (type (;6;) (func (result i32)))
  (type (;7;) (func (param i32) (result i64)))
  (type (;8;) (func (param i32 i32 i32 i32 i32)))
  (type (;9;) (func (param i32 i32 i32 i32)))
  (type (;10;) (func))
  (type (;11;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;12;) (func (param i32 i32 i32 i32 i32) (result i32)))
  (type (;13;) (func (param i32 i32 i32 i32 i32 i32)))
  (type (;14;) (func (param i32 i32 i32 i32 i32 i32) (result i32)))
  (type (;15;) (func (param i32 i32 f64 i32 i32)))
  (type (;16;) (func (param i32 f64 i32 i32)))
  (type (;17;) (func (param i32 i32 f32 i32 i32)))
  (type (;18;) (func (param i32 f32 i32 i32)))
  (type (;19;) (func (param i32 i32 i64 i32 i32)))
  (type (;20;) (func (param i32 i64 i32 i32)))
  (import "./wasm_game_of_life_bg.js" "__wbg_new_abda76e883ba8a5f" (func (;0;) (type 6)))
  (import "./wasm_game_of_life_bg.js" "__wbg_stack_658279fe44541cf6" (func (;1;) (type 1)))
  (import "./wasm_game_of_life_bg.js" "__wbg_error_f851667af71bcfc6" (func (;2;) (type 1)))
  (import "./wasm_game_of_life_bg.js" "__wbindgen_object_drop_ref" (func (;3;) (type 4)))
  (import "./wasm_game_of_life_bg.js" "__wbindgen_throw" (func (;4;) (type 1)))
  (func (;5;) (type 3) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 11
    global.set 0
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 0
                i32.const 245
                i32.ge_u
                if  ;; label = @7
                  i32.const 8
                  i32.const 8
                  call 66
                  local.set 6
                  i32.const 20
                  i32.const 8
                  call 66
                  local.set 5
                  i32.const 16
                  i32.const 8
                  call 66
                  local.set 1
                  i32.const 0
                  i32.const 16
                  i32.const 8
                  call 66
                  i32.const 2
                  i32.shl
                  i32.sub
                  local.tee 2
                  i32.const -65536
                  local.get 1
                  local.get 5
                  local.get 6
                  i32.add
                  i32.add
                  i32.sub
                  i32.const -9
                  i32.and
                  i32.const 3
                  i32.sub
                  local.tee 1
                  local.get 1
                  local.get 2
                  i32.gt_u
                  select
                  local.get 0
                  i32.le_u
                  br_if 6 (;@1;)
                  local.get 0
                  i32.const 4
                  i32.add
                  i32.const 8
                  call 66
                  local.set 4
                  i32.const 1050824
                  i32.load
                  i32.eqz
                  br_if 5 (;@2;)
                  i32.const 0
                  local.get 4
                  i32.sub
                  local.set 3
                  block (result i32)  ;; label = @8
                    i32.const 0
                    local.get 4
                    i32.const 256
                    i32.lt_u
                    br_if 0 (;@8;)
                    drop
                    i32.const 31
                    local.get 4
                    i32.const 16777215
                    i32.gt_u
                    br_if 0 (;@8;)
                    drop
                    local.get 4
                    i32.const 6
                    local.get 4
                    i32.const 8
                    i32.shr_u
                    i32.clz
                    local.tee 0
                    i32.sub
                    i32.shr_u
                    i32.const 1
                    i32.and
                    local.get 0
                    i32.const 1
                    i32.shl
                    i32.sub
                    i32.const 62
                    i32.add
                  end
                  local.tee 6
                  i32.const 2
                  i32.shl
                  i32.const 1050412
                  i32.add
                  i32.load
                  local.tee 1
                  br_if 1 (;@6;)
                  i32.const 0
                  local.set 0
                  i32.const 0
                  local.set 5
                  br 2 (;@5;)
                end
                i32.const 16
                local.get 0
                i32.const 4
                i32.add
                i32.const 16
                i32.const 8
                call 66
                i32.const 5
                i32.sub
                local.get 0
                i32.gt_u
                select
                i32.const 8
                call 66
                local.set 4
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block (result i32)  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            i32.const 1050820
                            i32.load
                            local.tee 1
                            local.get 4
                            i32.const 3
                            i32.shr_u
                            local.tee 0
                            i32.shr_u
                            local.tee 2
                            i32.const 3
                            i32.and
                            i32.eqz
                            if  ;; label = @13
                              local.get 4
                              i32.const 1050828
                              i32.load
                              i32.le_u
                              br_if 11 (;@2;)
                              local.get 2
                              br_if 1 (;@12;)
                              i32.const 1050824
                              i32.load
                              local.tee 0
                              i32.eqz
                              br_if 11 (;@2;)
                              local.get 0
                              call 77
                              i32.ctz
                              i32.const 2
                              i32.shl
                              i32.const 1050412
                              i32.add
                              i32.load
                              local.tee 1
                              call 86
                              local.get 4
                              i32.sub
                              local.set 3
                              local.get 1
                              call 63
                              local.tee 0
                              if  ;; label = @14
                                loop  ;; label = @15
                                  local.get 0
                                  call 86
                                  local.get 4
                                  i32.sub
                                  local.tee 2
                                  local.get 3
                                  local.get 2
                                  local.get 3
                                  i32.lt_u
                                  local.tee 2
                                  select
                                  local.set 3
                                  local.get 0
                                  local.get 1
                                  local.get 2
                                  select
                                  local.set 1
                                  local.get 0
                                  call 63
                                  local.tee 0
                                  br_if 0 (;@15;)
                                end
                              end
                              local.get 1
                              local.get 4
                              call 94
                              local.set 5
                              local.get 1
                              call 14
                              i32.const 16
                              i32.const 8
                              call 66
                              local.get 3
                              i32.gt_u
                              br_if 5 (;@8;)
                              local.get 1
                              local.get 4
                              call 79
                              local.get 5
                              local.get 3
                              call 65
                              i32.const 1050828
                              i32.load
                              local.tee 0
                              i32.eqz
                              br_if 4 (;@9;)
                              local.get 0
                              i32.const -8
                              i32.and
                              i32.const 1050556
                              i32.add
                              local.set 7
                              i32.const 1050836
                              i32.load
                              local.set 6
                              i32.const 1050820
                              i32.load
                              local.tee 2
                              i32.const 1
                              local.get 0
                              i32.const 3
                              i32.shr_u
                              i32.shl
                              local.tee 0
                              i32.and
                              i32.eqz
                              br_if 2 (;@11;)
                              local.get 7
                              i32.load offset=8
                              br 3 (;@10;)
                            end
                            block  ;; label = @13
                              local.get 2
                              i32.const -1
                              i32.xor
                              i32.const 1
                              i32.and
                              local.get 0
                              i32.add
                              local.tee 3
                              i32.const 3
                              i32.shl
                              local.tee 0
                              i32.const 1050564
                              i32.add
                              i32.load
                              local.tee 5
                              i32.const 8
                              i32.add
                              i32.load
                              local.tee 2
                              local.get 0
                              i32.const 1050556
                              i32.add
                              local.tee 0
                              i32.ne
                              if  ;; label = @14
                                local.get 2
                                local.get 0
                                i32.store offset=12
                                local.get 0
                                local.get 2
                                i32.store offset=8
                                br 1 (;@13;)
                              end
                              i32.const 1050820
                              local.get 1
                              i32.const -2
                              local.get 3
                              i32.rotl
                              i32.and
                              i32.store
                            end
                            local.get 5
                            local.get 3
                            i32.const 3
                            i32.shl
                            call 59
                            local.get 5
                            call 96
                            local.set 3
                            br 11 (;@1;)
                          end
                          block  ;; label = @12
                            i32.const 1
                            local.get 0
                            i32.const 31
                            i32.and
                            local.tee 0
                            i32.shl
                            call 69
                            local.get 2
                            local.get 0
                            i32.shl
                            i32.and
                            call 77
                            i32.ctz
                            local.tee 2
                            i32.const 3
                            i32.shl
                            local.tee 0
                            i32.const 1050564
                            i32.add
                            i32.load
                            local.tee 3
                            i32.const 8
                            i32.add
                            i32.load
                            local.tee 1
                            local.get 0
                            i32.const 1050556
                            i32.add
                            local.tee 0
                            i32.ne
                            if  ;; label = @13
                              local.get 1
                              local.get 0
                              i32.store offset=12
                              local.get 0
                              local.get 1
                              i32.store offset=8
                              br 1 (;@12;)
                            end
                            i32.const 1050820
                            i32.const 1050820
                            i32.load
                            i32.const -2
                            local.get 2
                            i32.rotl
                            i32.and
                            i32.store
                          end
                          local.get 3
                          local.get 4
                          call 79
                          local.get 3
                          local.get 4
                          call 94
                          local.tee 5
                          local.get 2
                          i32.const 3
                          i32.shl
                          local.get 4
                          i32.sub
                          local.tee 2
                          call 65
                          i32.const 1050828
                          i32.load
                          local.tee 0
                          if  ;; label = @12
                            local.get 0
                            i32.const -8
                            i32.and
                            i32.const 1050556
                            i32.add
                            local.set 7
                            i32.const 1050836
                            i32.load
                            local.set 6
                            block (result i32)  ;; label = @13
                              i32.const 1050820
                              i32.load
                              local.tee 1
                              i32.const 1
                              local.get 0
                              i32.const 3
                              i32.shr_u
                              i32.shl
                              local.tee 0
                              i32.and
                              if  ;; label = @14
                                local.get 7
                                i32.load offset=8
                                br 1 (;@13;)
                              end
                              i32.const 1050820
                              local.get 0
                              local.get 1
                              i32.or
                              i32.store
                              local.get 7
                            end
                            local.set 0
                            local.get 7
                            local.get 6
                            i32.store offset=8
                            local.get 0
                            local.get 6
                            i32.store offset=12
                            local.get 6
                            local.get 7
                            i32.store offset=12
                            local.get 6
                            local.get 0
                            i32.store offset=8
                          end
                          i32.const 1050836
                          local.get 5
                          i32.store
                          i32.const 1050828
                          local.get 2
                          i32.store
                          local.get 3
                          call 96
                          local.set 3
                          br 10 (;@1;)
                        end
                        i32.const 1050820
                        local.get 0
                        local.get 2
                        i32.or
                        i32.store
                        local.get 7
                      end
                      local.set 0
                      local.get 7
                      local.get 6
                      i32.store offset=8
                      local.get 0
                      local.get 6
                      i32.store offset=12
                      local.get 6
                      local.get 7
                      i32.store offset=12
                      local.get 6
                      local.get 0
                      i32.store offset=8
                    end
                    i32.const 1050836
                    local.get 5
                    i32.store
                    i32.const 1050828
                    local.get 3
                    i32.store
                    br 1 (;@7;)
                  end
                  local.get 1
                  local.get 3
                  local.get 4
                  i32.add
                  call 59
                end
                local.get 1
                call 96
                local.tee 3
                br_if 5 (;@1;)
                br 4 (;@2;)
              end
              local.get 4
              local.get 6
              call 64
              i32.shl
              local.set 7
              i32.const 0
              local.set 0
              i32.const 0
              local.set 5
              loop  ;; label = @6
                block  ;; label = @7
                  local.get 1
                  call 86
                  local.tee 2
                  local.get 4
                  i32.lt_u
                  br_if 0 (;@7;)
                  local.get 2
                  local.get 4
                  i32.sub
                  local.tee 2
                  local.get 3
                  i32.ge_u
                  br_if 0 (;@7;)
                  local.get 1
                  local.set 5
                  local.get 2
                  local.tee 3
                  br_if 0 (;@7;)
                  i32.const 0
                  local.set 3
                  local.get 1
                  local.set 0
                  br 3 (;@4;)
                end
                local.get 1
                i32.const 20
                i32.add
                i32.load
                local.tee 2
                local.get 0
                local.get 2
                local.get 1
                local.get 7
                i32.const 29
                i32.shr_u
                i32.const 4
                i32.and
                i32.add
                i32.const 16
                i32.add
                i32.load
                local.tee 1
                i32.ne
                select
                local.get 0
                local.get 2
                select
                local.set 0
                local.get 7
                i32.const 1
                i32.shl
                local.set 7
                local.get 1
                br_if 0 (;@6;)
              end
            end
            local.get 0
            local.get 5
            i32.or
            i32.eqz
            if  ;; label = @5
              i32.const 0
              local.set 5
              i32.const 1
              local.get 6
              i32.shl
              call 69
              i32.const 1050824
              i32.load
              i32.and
              local.tee 0
              i32.eqz
              br_if 3 (;@2;)
              local.get 0
              call 77
              i32.ctz
              i32.const 2
              i32.shl
              i32.const 1050412
              i32.add
              i32.load
              local.set 0
            end
            local.get 0
            i32.eqz
            br_if 1 (;@3;)
          end
          loop  ;; label = @4
            local.get 0
            local.get 5
            local.get 0
            call 86
            local.tee 1
            local.get 4
            i32.ge_u
            local.get 1
            local.get 4
            i32.sub
            local.tee 2
            local.get 3
            i32.lt_u
            i32.and
            local.tee 1
            select
            local.set 5
            local.get 2
            local.get 3
            local.get 1
            select
            local.set 3
            local.get 0
            call 63
            local.tee 0
            br_if 0 (;@4;)
          end
        end
        local.get 5
        i32.eqz
        br_if 0 (;@2;)
        local.get 4
        i32.const 1050828
        i32.load
        local.tee 0
        i32.le_u
        local.get 3
        local.get 0
        local.get 4
        i32.sub
        i32.ge_u
        i32.and
        br_if 0 (;@2;)
        local.get 5
        local.get 4
        call 94
        local.set 6
        local.get 5
        call 14
        block  ;; label = @3
          i32.const 16
          i32.const 8
          call 66
          local.get 3
          i32.le_u
          if  ;; label = @4
            local.get 5
            local.get 4
            call 79
            local.get 6
            local.get 3
            call 65
            local.get 3
            i32.const 256
            i32.ge_u
            if  ;; label = @5
              local.get 6
              local.get 3
              call 15
              br 2 (;@3;)
            end
            local.get 3
            i32.const -8
            i32.and
            i32.const 1050556
            i32.add
            local.set 2
            block (result i32)  ;; label = @5
              i32.const 1050820
              i32.load
              local.tee 1
              i32.const 1
              local.get 3
              i32.const 3
              i32.shr_u
              i32.shl
              local.tee 0
              i32.and
              if  ;; label = @6
                local.get 2
                i32.load offset=8
                br 1 (;@5;)
              end
              i32.const 1050820
              local.get 0
              local.get 1
              i32.or
              i32.store
              local.get 2
            end
            local.set 0
            local.get 2
            local.get 6
            i32.store offset=8
            local.get 0
            local.get 6
            i32.store offset=12
            local.get 6
            local.get 2
            i32.store offset=12
            local.get 6
            local.get 0
            i32.store offset=8
            br 1 (;@3;)
          end
          local.get 5
          local.get 3
          local.get 4
          i32.add
          call 59
        end
        local.get 5
        call 96
        local.tee 3
        br_if 1 (;@1;)
      end
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 4
                    i32.const 1050828
                    i32.load
                    local.tee 0
                    i32.gt_u
                    if  ;; label = @9
                      i32.const 1050832
                      i32.load
                      local.tee 0
                      local.get 4
                      i32.gt_u
                      br_if 2 (;@7;)
                      i32.const 8
                      i32.const 8
                      call 66
                      local.get 4
                      i32.add
                      i32.const 20
                      i32.const 8
                      call 66
                      i32.add
                      i32.const 16
                      i32.const 8
                      call 66
                      i32.add
                      i32.const 65536
                      call 66
                      local.tee 0
                      i32.const 16
                      i32.shr_u
                      memory.grow
                      local.set 1
                      local.get 11
                      i32.const 0
                      i32.store offset=8
                      local.get 11
                      i32.const 0
                      local.get 0
                      i32.const -65536
                      i32.and
                      local.get 1
                      i32.const -1
                      i32.eq
                      local.tee 0
                      select
                      i32.store offset=4
                      local.get 11
                      i32.const 0
                      local.get 1
                      i32.const 16
                      i32.shl
                      local.get 0
                      select
                      i32.store
                      local.get 11
                      i32.load
                      local.tee 8
                      br_if 1 (;@8;)
                      i32.const 0
                      local.set 3
                      br 8 (;@1;)
                    end
                    i32.const 1050836
                    i32.load
                    local.set 2
                    i32.const 16
                    i32.const 8
                    call 66
                    local.get 0
                    local.get 4
                    i32.sub
                    local.tee 1
                    i32.gt_u
                    if  ;; label = @9
                      i32.const 1050836
                      i32.const 0
                      i32.store
                      i32.const 1050828
                      i32.load
                      local.set 0
                      i32.const 1050828
                      i32.const 0
                      i32.store
                      local.get 2
                      local.get 0
                      call 59
                      local.get 2
                      call 96
                      local.set 3
                      br 8 (;@1;)
                    end
                    local.get 2
                    local.get 4
                    call 94
                    local.set 0
                    i32.const 1050828
                    local.get 1
                    i32.store
                    i32.const 1050836
                    local.get 0
                    i32.store
                    local.get 0
                    local.get 1
                    call 65
                    local.get 2
                    local.get 4
                    call 79
                    local.get 2
                    call 96
                    local.set 3
                    br 7 (;@1;)
                  end
                  local.get 11
                  i32.load offset=8
                  local.set 12
                  i32.const 1050844
                  local.get 11
                  i32.load offset=4
                  local.tee 10
                  i32.const 1050844
                  i32.load
                  i32.add
                  local.tee 1
                  i32.store
                  i32.const 1050848
                  i32.const 1050848
                  i32.load
                  local.tee 0
                  local.get 1
                  local.get 0
                  local.get 1
                  i32.gt_u
                  select
                  i32.store
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        i32.const 1050840
                        i32.load
                        if  ;; label = @11
                          i32.const 1050540
                          local.set 0
                          loop  ;; label = @12
                            local.get 0
                            call 80
                            local.get 8
                            i32.eq
                            br_if 2 (;@10;)
                            local.get 0
                            i32.load offset=8
                            local.tee 0
                            br_if 0 (;@12;)
                          end
                          br 2 (;@9;)
                        end
                        i32.const 1050856
                        i32.load
                        local.tee 0
                        i32.eqz
                        local.get 0
                        local.get 8
                        i32.gt_u
                        i32.or
                        br_if 5 (;@5;)
                        br 7 (;@3;)
                      end
                      local.get 0
                      call 88
                      br_if 0 (;@9;)
                      local.get 0
                      call 89
                      local.get 12
                      i32.ne
                      br_if 0 (;@9;)
                      local.get 0
                      i32.load
                      local.tee 2
                      i32.const 1050840
                      i32.load
                      local.tee 1
                      i32.le_u
                      if (result i32)  ;; label = @10
                        local.get 2
                        local.get 0
                        i32.load offset=4
                        i32.add
                        local.get 1
                        i32.gt_u
                      else
                        i32.const 0
                      end
                      br_if 1 (;@8;)
                    end
                    i32.const 1050856
                    i32.const 1050856
                    i32.load
                    local.tee 0
                    local.get 8
                    local.get 0
                    local.get 8
                    i32.lt_u
                    select
                    i32.store
                    local.get 8
                    local.get 10
                    i32.add
                    local.set 1
                    i32.const 1050540
                    local.set 0
                    block  ;; label = @9
                      block  ;; label = @10
                        loop  ;; label = @11
                          local.get 1
                          local.get 0
                          i32.load
                          i32.ne
                          if  ;; label = @12
                            local.get 0
                            i32.load offset=8
                            local.tee 0
                            br_if 1 (;@11;)
                            br 2 (;@10;)
                          end
                        end
                        local.get 0
                        call 88
                        br_if 0 (;@10;)
                        local.get 0
                        call 89
                        local.get 12
                        i32.eq
                        br_if 1 (;@9;)
                      end
                      i32.const 1050840
                      i32.load
                      local.set 9
                      i32.const 1050540
                      local.set 0
                      block  ;; label = @10
                        loop  ;; label = @11
                          local.get 9
                          local.get 0
                          i32.load
                          i32.ge_u
                          if  ;; label = @12
                            local.get 0
                            call 80
                            local.get 9
                            i32.gt_u
                            br_if 2 (;@10;)
                          end
                          local.get 0
                          i32.load offset=8
                          local.tee 0
                          br_if 0 (;@11;)
                        end
                        i32.const 0
                        local.set 0
                      end
                      local.get 9
                      local.get 0
                      call 80
                      local.tee 6
                      i32.const 20
                      i32.const 8
                      call 66
                      local.tee 15
                      i32.sub
                      i32.const 23
                      i32.sub
                      local.tee 1
                      call 96
                      local.tee 0
                      i32.const 8
                      call 66
                      local.get 0
                      i32.sub
                      local.get 1
                      i32.add
                      local.tee 0
                      local.get 0
                      i32.const 16
                      i32.const 8
                      call 66
                      local.get 9
                      i32.add
                      i32.lt_u
                      select
                      local.tee 13
                      call 96
                      local.set 14
                      local.get 13
                      local.get 15
                      call 94
                      local.set 0
                      i32.const 8
                      i32.const 8
                      call 66
                      local.set 3
                      i32.const 20
                      i32.const 8
                      call 66
                      local.set 5
                      i32.const 16
                      i32.const 8
                      call 66
                      local.set 2
                      i32.const 1050840
                      local.get 8
                      local.get 8
                      call 96
                      local.tee 1
                      i32.const 8
                      call 66
                      local.get 1
                      i32.sub
                      local.tee 1
                      call 94
                      local.tee 7
                      i32.store
                      i32.const 1050832
                      local.get 10
                      i32.const 8
                      i32.add
                      local.get 2
                      local.get 3
                      local.get 5
                      i32.add
                      i32.add
                      local.get 1
                      i32.add
                      i32.sub
                      local.tee 3
                      i32.store
                      local.get 7
                      local.get 3
                      i32.const 1
                      i32.or
                      i32.store offset=4
                      i32.const 8
                      i32.const 8
                      call 66
                      local.set 5
                      i32.const 20
                      i32.const 8
                      call 66
                      local.set 2
                      i32.const 16
                      i32.const 8
                      call 66
                      local.set 1
                      local.get 7
                      local.get 3
                      call 94
                      local.get 1
                      local.get 2
                      local.get 5
                      i32.const 8
                      i32.sub
                      i32.add
                      i32.add
                      i32.store offset=4
                      i32.const 1050852
                      i32.const 2097152
                      i32.store
                      local.get 13
                      local.get 15
                      call 79
                      i32.const 1050540
                      i64.load align=4
                      local.set 16
                      local.get 14
                      i32.const 8
                      i32.add
                      i32.const 1050548
                      i64.load align=4
                      i64.store align=4
                      local.get 14
                      local.get 16
                      i64.store align=4
                      i32.const 1050552
                      local.get 12
                      i32.store
                      i32.const 1050544
                      local.get 10
                      i32.store
                      i32.const 1050540
                      local.get 8
                      i32.store
                      i32.const 1050548
                      local.get 14
                      i32.store
                      loop  ;; label = @10
                        local.get 0
                        i32.const 4
                        call 94
                        local.get 0
                        i32.const 7
                        i32.store offset=4
                        local.tee 0
                        i32.const 4
                        i32.add
                        local.get 6
                        i32.lt_u
                        br_if 0 (;@10;)
                      end
                      local.get 9
                      local.get 13
                      i32.eq
                      br_if 7 (;@2;)
                      local.get 9
                      local.get 13
                      local.get 9
                      i32.sub
                      local.tee 0
                      local.get 9
                      local.get 0
                      call 94
                      call 52
                      local.get 0
                      i32.const 256
                      i32.ge_u
                      if  ;; label = @10
                        local.get 9
                        local.get 0
                        call 15
                        br 8 (;@2;)
                      end
                      local.get 0
                      i32.const -8
                      i32.and
                      i32.const 1050556
                      i32.add
                      local.set 2
                      block (result i32)  ;; label = @10
                        i32.const 1050820
                        i32.load
                        local.tee 1
                        i32.const 1
                        local.get 0
                        i32.const 3
                        i32.shr_u
                        i32.shl
                        local.tee 0
                        i32.and
                        if  ;; label = @11
                          local.get 2
                          i32.load offset=8
                          br 1 (;@10;)
                        end
                        i32.const 1050820
                        local.get 0
                        local.get 1
                        i32.or
                        i32.store
                        local.get 2
                      end
                      local.set 0
                      local.get 2
                      local.get 9
                      i32.store offset=8
                      local.get 0
                      local.get 9
                      i32.store offset=12
                      local.get 9
                      local.get 2
                      i32.store offset=12
                      local.get 9
                      local.get 0
                      i32.store offset=8
                      br 7 (;@2;)
                    end
                    local.get 0
                    i32.load
                    local.set 3
                    local.get 0
                    local.get 8
                    i32.store
                    local.get 0
                    local.get 0
                    i32.load offset=4
                    local.get 10
                    i32.add
                    i32.store offset=4
                    local.get 8
                    call 96
                    local.tee 5
                    i32.const 8
                    call 66
                    local.set 2
                    local.get 3
                    call 96
                    local.tee 1
                    i32.const 8
                    call 66
                    local.set 0
                    local.get 8
                    local.get 2
                    local.get 5
                    i32.sub
                    i32.add
                    local.tee 6
                    local.get 4
                    call 94
                    local.set 7
                    local.get 6
                    local.get 4
                    call 79
                    local.get 3
                    local.get 0
                    local.get 1
                    i32.sub
                    i32.add
                    local.tee 0
                    local.get 4
                    local.get 6
                    i32.add
                    i32.sub
                    local.set 4
                    i32.const 1050840
                    i32.load
                    local.get 0
                    i32.ne
                    if  ;; label = @9
                      local.get 0
                      i32.const 1050836
                      i32.load
                      i32.eq
                      br_if 3 (;@6;)
                      local.get 0
                      i32.load offset=4
                      i32.const 3
                      i32.and
                      i32.const 1
                      i32.ne
                      br_if 5 (;@4;)
                      block  ;; label = @10
                        local.get 0
                        call 86
                        local.tee 5
                        i32.const 256
                        i32.ge_u
                        if  ;; label = @11
                          local.get 0
                          call 14
                          br 1 (;@10;)
                        end
                        local.get 0
                        i32.const 12
                        i32.add
                        i32.load
                        local.tee 2
                        local.get 0
                        i32.const 8
                        i32.add
                        i32.load
                        local.tee 1
                        i32.ne
                        if  ;; label = @11
                          local.get 1
                          local.get 2
                          i32.store offset=12
                          local.get 2
                          local.get 1
                          i32.store offset=8
                          br 1 (;@10;)
                        end
                        i32.const 1050820
                        i32.const 1050820
                        i32.load
                        i32.const -2
                        local.get 5
                        i32.const 3
                        i32.shr_u
                        i32.rotl
                        i32.and
                        i32.store
                      end
                      local.get 4
                      local.get 5
                      i32.add
                      local.set 4
                      local.get 0
                      local.get 5
                      call 94
                      local.set 0
                      br 5 (;@4;)
                    end
                    i32.const 1050840
                    local.get 7
                    i32.store
                    i32.const 1050832
                    i32.const 1050832
                    i32.load
                    local.get 4
                    i32.add
                    local.tee 0
                    i32.store
                    local.get 7
                    local.get 0
                    i32.const 1
                    i32.or
                    i32.store offset=4
                    local.get 6
                    call 96
                    local.set 3
                    br 7 (;@1;)
                  end
                  local.get 0
                  local.get 0
                  i32.load offset=4
                  local.get 10
                  i32.add
                  i32.store offset=4
                  i32.const 1050832
                  i32.load
                  local.get 10
                  i32.add
                  local.set 1
                  i32.const 1050840
                  i32.load
                  local.tee 0
                  local.get 0
                  call 96
                  local.tee 0
                  i32.const 8
                  call 66
                  local.get 0
                  i32.sub
                  local.tee 0
                  call 94
                  local.set 3
                  i32.const 1050832
                  local.get 1
                  local.get 0
                  i32.sub
                  local.tee 5
                  i32.store
                  i32.const 1050840
                  local.get 3
                  i32.store
                  local.get 3
                  local.get 5
                  i32.const 1
                  i32.or
                  i32.store offset=4
                  i32.const 8
                  i32.const 8
                  call 66
                  local.set 2
                  i32.const 20
                  i32.const 8
                  call 66
                  local.set 1
                  i32.const 16
                  i32.const 8
                  call 66
                  local.set 0
                  local.get 3
                  local.get 5
                  call 94
                  local.get 0
                  local.get 1
                  local.get 2
                  i32.const 8
                  i32.sub
                  i32.add
                  i32.add
                  i32.store offset=4
                  i32.const 1050852
                  i32.const 2097152
                  i32.store
                  br 5 (;@2;)
                end
                i32.const 1050832
                local.get 0
                local.get 4
                i32.sub
                local.tee 1
                i32.store
                i32.const 1050840
                i32.const 1050840
                i32.load
                local.tee 2
                local.get 4
                call 94
                local.tee 0
                i32.store
                local.get 0
                local.get 1
                i32.const 1
                i32.or
                i32.store offset=4
                local.get 2
                local.get 4
                call 79
                local.get 2
                call 96
                local.set 3
                br 5 (;@1;)
              end
              i32.const 1050836
              local.get 7
              i32.store
              i32.const 1050828
              i32.const 1050828
              i32.load
              local.get 4
              i32.add
              local.tee 0
              i32.store
              local.get 7
              local.get 0
              call 65
              local.get 6
              call 96
              local.set 3
              br 4 (;@1;)
            end
            i32.const 1050856
            local.get 8
            i32.store
            br 1 (;@3;)
          end
          local.get 7
          local.get 4
          local.get 0
          call 52
          local.get 4
          i32.const 256
          i32.ge_u
          if  ;; label = @4
            local.get 7
            local.get 4
            call 15
            local.get 6
            call 96
            local.set 3
            br 3 (;@1;)
          end
          local.get 4
          i32.const -8
          i32.and
          i32.const 1050556
          i32.add
          local.set 2
          block (result i32)  ;; label = @4
            i32.const 1050820
            i32.load
            local.tee 1
            i32.const 1
            local.get 4
            i32.const 3
            i32.shr_u
            i32.shl
            local.tee 0
            i32.and
            if  ;; label = @5
              local.get 2
              i32.load offset=8
              br 1 (;@4;)
            end
            i32.const 1050820
            local.get 0
            local.get 1
            i32.or
            i32.store
            local.get 2
          end
          local.set 0
          local.get 2
          local.get 7
          i32.store offset=8
          local.get 0
          local.get 7
          i32.store offset=12
          local.get 7
          local.get 2
          i32.store offset=12
          local.get 7
          local.get 0
          i32.store offset=8
          local.get 6
          call 96
          local.set 3
          br 2 (;@1;)
        end
        i32.const 1050860
        i32.const 4095
        i32.store
        i32.const 1050552
        local.get 12
        i32.store
        i32.const 1050544
        local.get 10
        i32.store
        i32.const 1050540
        local.get 8
        i32.store
        i32.const 1050568
        i32.const 1050556
        i32.store
        i32.const 1050576
        i32.const 1050564
        i32.store
        i32.const 1050564
        i32.const 1050556
        i32.store
        i32.const 1050584
        i32.const 1050572
        i32.store
        i32.const 1050572
        i32.const 1050564
        i32.store
        i32.const 1050592
        i32.const 1050580
        i32.store
        i32.const 1050580
        i32.const 1050572
        i32.store
        i32.const 1050600
        i32.const 1050588
        i32.store
        i32.const 1050588
        i32.const 1050580
        i32.store
        i32.const 1050608
        i32.const 1050596
        i32.store
        i32.const 1050596
        i32.const 1050588
        i32.store
        i32.const 1050616
        i32.const 1050604
        i32.store
        i32.const 1050604
        i32.const 1050596
        i32.store
        i32.const 1050624
        i32.const 1050612
        i32.store
        i32.const 1050612
        i32.const 1050604
        i32.store
        i32.const 1050632
        i32.const 1050620
        i32.store
        i32.const 1050620
        i32.const 1050612
        i32.store
        i32.const 1050628
        i32.const 1050620
        i32.store
        i32.const 1050640
        i32.const 1050628
        i32.store
        i32.const 1050636
        i32.const 1050628
        i32.store
        i32.const 1050648
        i32.const 1050636
        i32.store
        i32.const 1050644
        i32.const 1050636
        i32.store
        i32.const 1050656
        i32.const 1050644
        i32.store
        i32.const 1050652
        i32.const 1050644
        i32.store
        i32.const 1050664
        i32.const 1050652
        i32.store
        i32.const 1050660
        i32.const 1050652
        i32.store
        i32.const 1050672
        i32.const 1050660
        i32.store
        i32.const 1050668
        i32.const 1050660
        i32.store
        i32.const 1050680
        i32.const 1050668
        i32.store
        i32.const 1050676
        i32.const 1050668
        i32.store
        i32.const 1050688
        i32.const 1050676
        i32.store
        i32.const 1050684
        i32.const 1050676
        i32.store
        i32.const 1050696
        i32.const 1050684
        i32.store
        i32.const 1050704
        i32.const 1050692
        i32.store
        i32.const 1050692
        i32.const 1050684
        i32.store
        i32.const 1050712
        i32.const 1050700
        i32.store
        i32.const 1050700
        i32.const 1050692
        i32.store
        i32.const 1050720
        i32.const 1050708
        i32.store
        i32.const 1050708
        i32.const 1050700
        i32.store
        i32.const 1050728
        i32.const 1050716
        i32.store
        i32.const 1050716
        i32.const 1050708
        i32.store
        i32.const 1050736
        i32.const 1050724
        i32.store
        i32.const 1050724
        i32.const 1050716
        i32.store
        i32.const 1050744
        i32.const 1050732
        i32.store
        i32.const 1050732
        i32.const 1050724
        i32.store
        i32.const 1050752
        i32.const 1050740
        i32.store
        i32.const 1050740
        i32.const 1050732
        i32.store
        i32.const 1050760
        i32.const 1050748
        i32.store
        i32.const 1050748
        i32.const 1050740
        i32.store
        i32.const 1050768
        i32.const 1050756
        i32.store
        i32.const 1050756
        i32.const 1050748
        i32.store
        i32.const 1050776
        i32.const 1050764
        i32.store
        i32.const 1050764
        i32.const 1050756
        i32.store
        i32.const 1050784
        i32.const 1050772
        i32.store
        i32.const 1050772
        i32.const 1050764
        i32.store
        i32.const 1050792
        i32.const 1050780
        i32.store
        i32.const 1050780
        i32.const 1050772
        i32.store
        i32.const 1050800
        i32.const 1050788
        i32.store
        i32.const 1050788
        i32.const 1050780
        i32.store
        i32.const 1050808
        i32.const 1050796
        i32.store
        i32.const 1050796
        i32.const 1050788
        i32.store
        i32.const 1050816
        i32.const 1050804
        i32.store
        i32.const 1050804
        i32.const 1050796
        i32.store
        i32.const 1050812
        i32.const 1050804
        i32.store
        i32.const 8
        i32.const 8
        call 66
        local.set 5
        i32.const 20
        i32.const 8
        call 66
        local.set 2
        i32.const 16
        i32.const 8
        call 66
        local.set 1
        i32.const 1050840
        local.get 8
        local.get 8
        call 96
        local.tee 0
        i32.const 8
        call 66
        local.get 0
        i32.sub
        local.tee 0
        call 94
        local.tee 3
        i32.store
        i32.const 1050832
        local.get 10
        i32.const 8
        i32.add
        local.get 1
        local.get 2
        local.get 5
        i32.add
        i32.add
        local.get 0
        i32.add
        i32.sub
        local.tee 5
        i32.store
        local.get 3
        local.get 5
        i32.const 1
        i32.or
        i32.store offset=4
        i32.const 8
        i32.const 8
        call 66
        local.set 2
        i32.const 20
        i32.const 8
        call 66
        local.set 1
        i32.const 16
        i32.const 8
        call 66
        local.set 0
        local.get 3
        local.get 5
        call 94
        local.get 0
        local.get 1
        local.get 2
        i32.const 8
        i32.sub
        i32.add
        i32.add
        i32.store offset=4
        i32.const 1050852
        i32.const 2097152
        i32.store
      end
      i32.const 0
      local.set 3
      i32.const 1050832
      i32.load
      local.tee 0
      local.get 4
      i32.le_u
      br_if 0 (;@1;)
      i32.const 1050832
      local.get 0
      local.get 4
      i32.sub
      local.tee 1
      i32.store
      i32.const 1050840
      i32.const 1050840
      i32.load
      local.tee 2
      local.get 4
      call 94
      local.tee 0
      i32.store
      local.get 0
      local.get 1
      i32.const 1
      i32.or
      i32.store offset=4
      local.get 2
      local.get 4
      call 79
      local.get 2
      call 96
      local.set 3
    end
    local.get 11
    i32.const 16
    i32.add
    global.set 0
    local.get 3)
  (func (;6;) (type 4) (param i32)
    (local i32 i32 i32 i32 i32)
    local.get 0
    call 97
    local.tee 0
    local.get 0
    call 86
    local.tee 2
    call 94
    local.set 1
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          call 87
          br_if 0 (;@3;)
          local.get 0
          i32.load
          local.set 3
          block  ;; label = @4
            local.get 0
            call 78
            i32.eqz
            if  ;; label = @5
              local.get 2
              local.get 3
              i32.add
              local.set 2
              local.get 0
              local.get 3
              call 95
              local.tee 0
              i32.const 1050836
              i32.load
              i32.ne
              br_if 1 (;@4;)
              local.get 1
              i32.load offset=4
              i32.const 3
              i32.and
              i32.const 3
              i32.ne
              br_if 2 (;@3;)
              i32.const 1050828
              local.get 2
              i32.store
              local.get 0
              local.get 2
              local.get 1
              call 52
              return
            end
            local.get 2
            local.get 3
            i32.add
            i32.const 16
            i32.add
            local.set 0
            br 2 (;@2;)
          end
          local.get 3
          i32.const 256
          i32.ge_u
          if  ;; label = @4
            local.get 0
            call 14
            br 1 (;@3;)
          end
          local.get 0
          i32.const 12
          i32.add
          i32.load
          local.tee 4
          local.get 0
          i32.const 8
          i32.add
          i32.load
          local.tee 5
          i32.ne
          if  ;; label = @4
            local.get 5
            local.get 4
            i32.store offset=12
            local.get 4
            local.get 5
            i32.store offset=8
            br 1 (;@3;)
          end
          i32.const 1050820
          i32.const 1050820
          i32.load
          i32.const -2
          local.get 3
          i32.const 3
          i32.shr_u
          i32.rotl
          i32.and
          i32.store
        end
        block  ;; label = @3
          local.get 1
          call 75
          if  ;; label = @4
            local.get 0
            local.get 2
            local.get 1
            call 52
            br 1 (;@3;)
          end
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                i32.const 1050840
                i32.load
                local.get 1
                i32.ne
                if  ;; label = @7
                  local.get 1
                  i32.const 1050836
                  i32.load
                  i32.ne
                  br_if 1 (;@6;)
                  i32.const 1050836
                  local.get 0
                  i32.store
                  i32.const 1050828
                  i32.const 1050828
                  i32.load
                  local.get 2
                  i32.add
                  local.tee 1
                  i32.store
                  local.get 0
                  local.get 1
                  call 65
                  return
                end
                i32.const 1050840
                local.get 0
                i32.store
                i32.const 1050832
                i32.const 1050832
                i32.load
                local.get 2
                i32.add
                local.tee 1
                i32.store
                local.get 0
                local.get 1
                i32.const 1
                i32.or
                i32.store offset=4
                local.get 0
                i32.const 1050836
                i32.load
                i32.eq
                br_if 1 (;@5;)
                br 2 (;@4;)
              end
              local.get 1
              call 86
              local.tee 3
              local.get 2
              i32.add
              local.set 2
              block  ;; label = @6
                local.get 3
                i32.const 256
                i32.ge_u
                if  ;; label = @7
                  local.get 1
                  call 14
                  br 1 (;@6;)
                end
                local.get 1
                i32.const 12
                i32.add
                i32.load
                local.tee 4
                local.get 1
                i32.const 8
                i32.add
                i32.load
                local.tee 1
                i32.ne
                if  ;; label = @7
                  local.get 1
                  local.get 4
                  i32.store offset=12
                  local.get 4
                  local.get 1
                  i32.store offset=8
                  br 1 (;@6;)
                end
                i32.const 1050820
                i32.const 1050820
                i32.load
                i32.const -2
                local.get 3
                i32.const 3
                i32.shr_u
                i32.rotl
                i32.and
                i32.store
              end
              local.get 0
              local.get 2
              call 65
              local.get 0
              i32.const 1050836
              i32.load
              i32.ne
              br_if 2 (;@3;)
              i32.const 1050828
              local.get 2
              i32.store
              br 3 (;@2;)
            end
            i32.const 1050828
            i32.const 0
            i32.store
            i32.const 1050836
            i32.const 0
            i32.store
          end
          i32.const 1050852
          i32.load
          local.get 1
          i32.ge_u
          br_if 1 (;@2;)
          i32.const 8
          i32.const 8
          call 66
          local.set 0
          i32.const 20
          i32.const 8
          call 66
          local.set 1
          i32.const 16
          i32.const 8
          call 66
          local.set 3
          i32.const 0
          i32.const 16
          i32.const 8
          call 66
          i32.const 2
          i32.shl
          i32.sub
          local.tee 2
          i32.const -65536
          local.get 3
          local.get 0
          local.get 1
          i32.add
          i32.add
          i32.sub
          i32.const -9
          i32.and
          i32.const 3
          i32.sub
          local.tee 0
          local.get 0
          local.get 2
          i32.gt_u
          select
          i32.eqz
          br_if 1 (;@2;)
          i32.const 1050840
          i32.load
          i32.eqz
          br_if 1 (;@2;)
          i32.const 8
          i32.const 8
          call 66
          local.set 0
          i32.const 20
          i32.const 8
          call 66
          local.set 1
          i32.const 16
          i32.const 8
          call 66
          local.set 2
          i32.const 0
          block  ;; label = @4
            i32.const 1050832
            i32.load
            local.tee 4
            local.get 2
            local.get 1
            local.get 0
            i32.const 8
            i32.sub
            i32.add
            i32.add
            local.tee 2
            i32.le_u
            br_if 0 (;@4;)
            i32.const 1050840
            i32.load
            local.set 1
            i32.const 1050540
            local.set 0
            block  ;; label = @5
              loop  ;; label = @6
                local.get 1
                local.get 0
                i32.load
                i32.ge_u
                if  ;; label = @7
                  local.get 0
                  call 80
                  local.get 1
                  i32.gt_u
                  br_if 2 (;@5;)
                end
                local.get 0
                i32.load offset=8
                local.tee 0
                br_if 0 (;@6;)
              end
              i32.const 0
              local.set 0
            end
            local.get 0
            call 88
            br_if 0 (;@4;)
            local.get 0
            i32.const 12
            i32.add
            i32.load
            drop
            br 0 (;@4;)
          end
          i32.const 0
          call 16
          i32.sub
          i32.ne
          br_if 1 (;@2;)
          i32.const 1050832
          i32.load
          i32.const 1050852
          i32.load
          i32.le_u
          br_if 1 (;@2;)
          i32.const 1050852
          i32.const -1
          i32.store
          return
        end
        local.get 2
        i32.const 256
        i32.lt_u
        br_if 1 (;@1;)
        local.get 0
        local.get 2
        call 15
        i32.const 1050860
        i32.const 1050860
        i32.load
        i32.const 1
        i32.sub
        local.tee 0
        i32.store
        local.get 0
        br_if 0 (;@2;)
        call 16
        drop
        return
      end
      return
    end
    local.get 2
    i32.const -8
    i32.and
    i32.const 1050556
    i32.add
    local.set 1
    block (result i32)  ;; label = @1
      i32.const 1050820
      i32.load
      local.tee 3
      i32.const 1
      local.get 2
      i32.const 3
      i32.shr_u
      i32.shl
      local.tee 2
      i32.and
      if  ;; label = @2
        local.get 1
        i32.load offset=8
        br 1 (;@1;)
      end
      i32.const 1050820
      local.get 2
      local.get 3
      i32.or
      i32.store
      local.get 1
    end
    local.set 3
    local.get 1
    local.get 0
    i32.store offset=8
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 0
    local.get 1
    i32.store offset=12
    local.get 0
    local.get 3
    i32.store offset=8)
  (func (;7;) (type 2) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load offset=8
        local.tee 10
        i32.const 1
        i32.ne
        local.get 0
        i32.load offset=16
        local.tee 3
        i32.const 1
        i32.ne
        i32.and
        i32.eqz
        if  ;; label = @3
          block  ;; label = @4
            local.get 3
            i32.const 1
            i32.ne
            br_if 0 (;@4;)
            local.get 1
            local.get 2
            i32.add
            local.set 8
            local.get 0
            i32.const 20
            i32.add
            i32.load
            i32.const 1
            i32.add
            local.set 6
            local.get 1
            local.set 4
            loop  ;; label = @5
              block  ;; label = @6
                local.get 4
                local.set 3
                local.get 6
                i32.const 1
                i32.sub
                local.tee 6
                i32.eqz
                br_if 0 (;@6;)
                local.get 3
                local.get 8
                i32.eq
                br_if 2 (;@4;)
                block (result i32)  ;; label = @7
                  local.get 3
                  i32.load8_s
                  local.tee 4
                  i32.const 0
                  i32.ge_s
                  if  ;; label = @8
                    local.get 4
                    i32.const 255
                    i32.and
                    local.set 5
                    local.get 3
                    i32.const 1
                    i32.add
                    br 1 (;@7;)
                  end
                  local.get 3
                  i32.load8_u offset=1
                  i32.const 63
                  i32.and
                  local.set 9
                  local.get 4
                  i32.const 31
                  i32.and
                  local.set 5
                  local.get 4
                  i32.const -33
                  i32.le_u
                  if  ;; label = @8
                    local.get 5
                    i32.const 6
                    i32.shl
                    local.get 9
                    i32.or
                    local.set 5
                    local.get 3
                    i32.const 2
                    i32.add
                    br 1 (;@7;)
                  end
                  local.get 3
                  i32.load8_u offset=2
                  i32.const 63
                  i32.and
                  local.get 9
                  i32.const 6
                  i32.shl
                  i32.or
                  local.set 9
                  local.get 4
                  i32.const -16
                  i32.lt_u
                  if  ;; label = @8
                    local.get 9
                    local.get 5
                    i32.const 12
                    i32.shl
                    i32.or
                    local.set 5
                    local.get 3
                    i32.const 3
                    i32.add
                    br 1 (;@7;)
                  end
                  local.get 5
                  i32.const 18
                  i32.shl
                  i32.const 1835008
                  i32.and
                  local.get 3
                  i32.load8_u offset=3
                  i32.const 63
                  i32.and
                  local.get 9
                  i32.const 6
                  i32.shl
                  i32.or
                  i32.or
                  local.tee 5
                  i32.const 1114112
                  i32.eq
                  br_if 3 (;@4;)
                  local.get 3
                  i32.const 4
                  i32.add
                end
                local.tee 4
                local.get 7
                local.get 3
                i32.sub
                i32.add
                local.set 7
                local.get 5
                i32.const 1114112
                i32.ne
                br_if 1 (;@5;)
                br 2 (;@4;)
              end
            end
            local.get 3
            local.get 8
            i32.eq
            br_if 0 (;@4;)
            local.get 3
            i32.load8_s
            local.tee 4
            i32.const 0
            i32.ge_s
            local.get 4
            i32.const -32
            i32.lt_u
            i32.or
            local.get 4
            i32.const -16
            i32.lt_u
            i32.or
            i32.eqz
            if  ;; label = @5
              local.get 4
              i32.const 255
              i32.and
              i32.const 18
              i32.shl
              i32.const 1835008
              i32.and
              local.get 3
              i32.load8_u offset=3
              i32.const 63
              i32.and
              local.get 3
              i32.load8_u offset=2
              i32.const 63
              i32.and
              i32.const 6
              i32.shl
              local.get 3
              i32.load8_u offset=1
              i32.const 63
              i32.and
              i32.const 12
              i32.shl
              i32.or
              i32.or
              i32.or
              i32.const 1114112
              i32.eq
              br_if 1 (;@4;)
            end
            block  ;; label = @5
              block  ;; label = @6
                local.get 7
                i32.eqz
                br_if 0 (;@6;)
                local.get 2
                local.get 7
                i32.le_u
                if  ;; label = @7
                  i32.const 0
                  local.set 3
                  local.get 2
                  local.get 7
                  i32.eq
                  br_if 1 (;@6;)
                  br 2 (;@5;)
                end
                i32.const 0
                local.set 3
                local.get 1
                local.get 7
                i32.add
                i32.load8_s
                i32.const -64
                i32.lt_s
                br_if 1 (;@5;)
              end
              local.get 1
              local.set 3
            end
            local.get 7
            local.get 2
            local.get 3
            select
            local.set 2
            local.get 3
            local.get 1
            local.get 3
            select
            local.set 1
          end
          local.get 10
          i32.eqz
          br_if 2 (;@1;)
          local.get 0
          i32.const 12
          i32.add
          i32.load
          local.set 12
          block  ;; label = @4
            local.get 2
            i32.const 16
            i32.ge_u
            if  ;; label = @5
              block (result i32)  ;; label = @6
                i32.const 0
                local.set 5
                i32.const 0
                local.set 7
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 1
                    local.tee 3
                    i32.const 3
                    i32.add
                    i32.const -4
                    i32.and
                    local.tee 4
                    local.get 3
                    i32.sub
                    local.tee 8
                    local.get 2
                    i32.gt_u
                    local.get 8
                    i32.const 4
                    i32.gt_u
                    i32.or
                    br_if 0 (;@8;)
                    local.get 2
                    local.get 8
                    i32.sub
                    local.tee 10
                    i32.const 4
                    i32.lt_u
                    br_if 0 (;@8;)
                    local.get 10
                    i32.const 3
                    i32.and
                    local.set 11
                    i32.const 0
                    local.set 6
                    block  ;; label = @9
                      local.get 3
                      local.get 4
                      i32.eq
                      br_if 0 (;@9;)
                      local.get 8
                      i32.const 3
                      i32.and
                      local.set 5
                      block  ;; label = @10
                        local.get 4
                        local.get 3
                        i32.const -1
                        i32.xor
                        i32.add
                        i32.const 3
                        i32.lt_u
                        if  ;; label = @11
                          local.get 3
                          local.set 4
                          br 1 (;@10;)
                        end
                        local.get 8
                        i32.const -4
                        i32.and
                        local.set 9
                        local.get 3
                        local.set 4
                        loop  ;; label = @11
                          local.get 6
                          local.get 4
                          i32.load8_s
                          i32.const -65
                          i32.gt_s
                          i32.add
                          local.get 4
                          i32.load8_s offset=1
                          i32.const -65
                          i32.gt_s
                          i32.add
                          local.get 4
                          i32.load8_s offset=2
                          i32.const -65
                          i32.gt_s
                          i32.add
                          local.get 4
                          i32.load8_s offset=3
                          i32.const -65
                          i32.gt_s
                          i32.add
                          local.set 6
                          local.get 4
                          i32.const 4
                          i32.add
                          local.set 4
                          local.get 9
                          i32.const 4
                          i32.sub
                          local.tee 9
                          br_if 0 (;@11;)
                        end
                      end
                      local.get 5
                      i32.eqz
                      br_if 0 (;@9;)
                      loop  ;; label = @10
                        local.get 6
                        local.get 4
                        i32.load8_s
                        i32.const -65
                        i32.gt_s
                        i32.add
                        local.set 6
                        local.get 4
                        i32.const 1
                        i32.add
                        local.set 4
                        local.get 5
                        i32.const 1
                        i32.sub
                        local.tee 5
                        br_if 0 (;@10;)
                      end
                    end
                    local.get 3
                    local.get 8
                    i32.add
                    local.set 3
                    block  ;; label = @9
                      local.get 11
                      i32.eqz
                      br_if 0 (;@9;)
                      local.get 3
                      local.get 10
                      i32.const -4
                      i32.and
                      i32.add
                      local.tee 4
                      i32.load8_s
                      i32.const -65
                      i32.gt_s
                      local.set 7
                      local.get 11
                      i32.const 1
                      i32.eq
                      br_if 0 (;@9;)
                      local.get 7
                      local.get 4
                      i32.load8_s offset=1
                      i32.const -65
                      i32.gt_s
                      i32.add
                      local.set 7
                      local.get 11
                      i32.const 2
                      i32.eq
                      br_if 0 (;@9;)
                      local.get 7
                      local.get 4
                      i32.load8_s offset=2
                      i32.const -65
                      i32.gt_s
                      i32.add
                      local.set 7
                    end
                    local.get 10
                    i32.const 2
                    i32.shr_u
                    local.set 8
                    local.get 6
                    local.get 7
                    i32.add
                    local.set 5
                    loop  ;; label = @9
                      local.get 3
                      local.set 6
                      local.get 8
                      i32.eqz
                      br_if 2 (;@7;)
                      i32.const 192
                      local.get 8
                      local.get 8
                      i32.const 192
                      i32.ge_u
                      select
                      local.tee 7
                      i32.const 3
                      i32.and
                      local.set 9
                      local.get 7
                      i32.const 2
                      i32.shl
                      local.set 11
                      block  ;; label = @10
                        local.get 7
                        i32.const 252
                        i32.and
                        local.tee 10
                        i32.eqz
                        if  ;; label = @11
                          i32.const 0
                          local.set 4
                          br 1 (;@10;)
                        end
                        local.get 6
                        local.get 10
                        i32.const 2
                        i32.shl
                        i32.add
                        local.set 13
                        i32.const 0
                        local.set 4
                        loop  ;; label = @11
                          local.get 3
                          i32.eqz
                          br_if 1 (;@10;)
                          local.get 4
                          local.get 3
                          i32.load
                          local.tee 14
                          i32.const -1
                          i32.xor
                          i32.const 7
                          i32.shr_u
                          local.get 14
                          i32.const 6
                          i32.shr_u
                          i32.or
                          i32.const 16843009
                          i32.and
                          i32.add
                          local.get 3
                          i32.const 4
                          i32.add
                          i32.load
                          local.tee 4
                          i32.const -1
                          i32.xor
                          i32.const 7
                          i32.shr_u
                          local.get 4
                          i32.const 6
                          i32.shr_u
                          i32.or
                          i32.const 16843009
                          i32.and
                          i32.add
                          local.get 3
                          i32.const 8
                          i32.add
                          i32.load
                          local.tee 4
                          i32.const -1
                          i32.xor
                          i32.const 7
                          i32.shr_u
                          local.get 4
                          i32.const 6
                          i32.shr_u
                          i32.or
                          i32.const 16843009
                          i32.and
                          i32.add
                          local.get 3
                          i32.const 12
                          i32.add
                          i32.load
                          local.tee 4
                          i32.const -1
                          i32.xor
                          i32.const 7
                          i32.shr_u
                          local.get 4
                          i32.const 6
                          i32.shr_u
                          i32.or
                          i32.const 16843009
                          i32.and
                          i32.add
                          local.set 4
                          local.get 3
                          i32.const 16
                          i32.add
                          local.tee 3
                          local.get 13
                          i32.ne
                          br_if 0 (;@11;)
                        end
                      end
                      local.get 8
                      local.get 7
                      i32.sub
                      local.set 8
                      local.get 6
                      local.get 11
                      i32.add
                      local.set 3
                      local.get 4
                      i32.const 8
                      i32.shr_u
                      i32.const 16711935
                      i32.and
                      local.get 4
                      i32.const 16711935
                      i32.and
                      i32.add
                      i32.const 65537
                      i32.mul
                      i32.const 16
                      i32.shr_u
                      local.get 5
                      i32.add
                      local.set 5
                      local.get 9
                      i32.eqz
                      br_if 0 (;@9;)
                    end
                    block  ;; label = @9
                      local.get 6
                      i32.eqz
                      if  ;; label = @10
                        i32.const 0
                        local.set 4
                        br 1 (;@9;)
                      end
                      local.get 6
                      local.get 10
                      i32.const 2
                      i32.shl
                      i32.add
                      local.set 3
                      local.get 9
                      i32.const 1
                      i32.sub
                      i32.const 1073741823
                      i32.and
                      local.tee 4
                      i32.const 1
                      i32.add
                      local.tee 7
                      i32.const 3
                      i32.and
                      local.set 6
                      block  ;; label = @10
                        local.get 4
                        i32.const 3
                        i32.lt_u
                        if  ;; label = @11
                          i32.const 0
                          local.set 4
                          br 1 (;@10;)
                        end
                        local.get 7
                        i32.const 2147483644
                        i32.and
                        local.set 9
                        i32.const 0
                        local.set 4
                        loop  ;; label = @11
                          local.get 4
                          local.get 3
                          i32.load
                          local.tee 7
                          i32.const -1
                          i32.xor
                          i32.const 7
                          i32.shr_u
                          local.get 7
                          i32.const 6
                          i32.shr_u
                          i32.or
                          i32.const 16843009
                          i32.and
                          i32.add
                          local.get 3
                          i32.const 4
                          i32.add
                          i32.load
                          local.tee 4
                          i32.const -1
                          i32.xor
                          i32.const 7
                          i32.shr_u
                          local.get 4
                          i32.const 6
                          i32.shr_u
                          i32.or
                          i32.const 16843009
                          i32.and
                          i32.add
                          local.get 3
                          i32.const 8
                          i32.add
                          i32.load
                          local.tee 4
                          i32.const -1
                          i32.xor
                          i32.const 7
                          i32.shr_u
                          local.get 4
                          i32.const 6
                          i32.shr_u
                          i32.or
                          i32.const 16843009
                          i32.and
                          i32.add
                          local.get 3
                          i32.const 12
                          i32.add
                          i32.load
                          local.tee 4
                          i32.const -1
                          i32.xor
                          i32.const 7
                          i32.shr_u
                          local.get 4
                          i32.const 6
                          i32.shr_u
                          i32.or
                          i32.const 16843009
                          i32.and
                          i32.add
                          local.set 4
                          local.get 3
                          i32.const 16
                          i32.add
                          local.set 3
                          local.get 9
                          i32.const 4
                          i32.sub
                          local.tee 9
                          br_if 0 (;@11;)
                        end
                      end
                      local.get 6
                      i32.eqz
                      br_if 0 (;@9;)
                      loop  ;; label = @10
                        local.get 4
                        local.get 3
                        i32.load
                        local.tee 7
                        i32.const -1
                        i32.xor
                        i32.const 7
                        i32.shr_u
                        local.get 7
                        i32.const 6
                        i32.shr_u
                        i32.or
                        i32.const 16843009
                        i32.and
                        i32.add
                        local.set 4
                        local.get 3
                        i32.const 4
                        i32.add
                        local.set 3
                        local.get 6
                        i32.const 1
                        i32.sub
                        local.tee 6
                        br_if 0 (;@10;)
                      end
                    end
                    local.get 4
                    i32.const 8
                    i32.shr_u
                    i32.const 16711935
                    i32.and
                    local.get 4
                    i32.const 16711935
                    i32.and
                    i32.add
                    i32.const 65537
                    i32.mul
                    i32.const 16
                    i32.shr_u
                    local.get 5
                    i32.add
                    br 2 (;@6;)
                  end
                  i32.const 0
                  local.get 2
                  i32.eqz
                  br_if 1 (;@6;)
                  drop
                  local.get 2
                  i32.const 3
                  i32.and
                  local.set 4
                  local.get 2
                  i32.const 1
                  i32.sub
                  i32.const 3
                  i32.ge_u
                  if  ;; label = @8
                    local.get 2
                    i32.const -4
                    i32.and
                    local.set 6
                    loop  ;; label = @9
                      local.get 5
                      local.get 3
                      i32.load8_s
                      i32.const -65
                      i32.gt_s
                      i32.add
                      local.get 3
                      i32.load8_s offset=1
                      i32.const -65
                      i32.gt_s
                      i32.add
                      local.get 3
                      i32.load8_s offset=2
                      i32.const -65
                      i32.gt_s
                      i32.add
                      local.get 3
                      i32.load8_s offset=3
                      i32.const -65
                      i32.gt_s
                      i32.add
                      local.set 5
                      local.get 3
                      i32.const 4
                      i32.add
                      local.set 3
                      local.get 6
                      i32.const 4
                      i32.sub
                      local.tee 6
                      br_if 0 (;@9;)
                    end
                  end
                  local.get 4
                  i32.eqz
                  br_if 0 (;@7;)
                  loop  ;; label = @8
                    local.get 5
                    local.get 3
                    i32.load8_s
                    i32.const -65
                    i32.gt_s
                    i32.add
                    local.set 5
                    local.get 3
                    i32.const 1
                    i32.add
                    local.set 3
                    local.get 4
                    i32.const 1
                    i32.sub
                    local.tee 4
                    br_if 0 (;@8;)
                  end
                end
                local.get 5
              end
              local.set 4
              br 1 (;@4;)
            end
            local.get 2
            i32.eqz
            if  ;; label = @5
              i32.const 0
              local.set 4
              br 1 (;@4;)
            end
            local.get 2
            i32.const 3
            i32.and
            local.set 5
            block  ;; label = @5
              local.get 2
              i32.const 1
              i32.sub
              i32.const 3
              i32.lt_u
              if  ;; label = @6
                i32.const 0
                local.set 4
                local.get 1
                local.set 3
                br 1 (;@5;)
              end
              local.get 2
              i32.const -4
              i32.and
              local.set 6
              i32.const 0
              local.set 4
              local.get 1
              local.set 3
              loop  ;; label = @6
                local.get 4
                local.get 3
                i32.load8_s
                i32.const -65
                i32.gt_s
                i32.add
                local.get 3
                i32.load8_s offset=1
                i32.const -65
                i32.gt_s
                i32.add
                local.get 3
                i32.load8_s offset=2
                i32.const -65
                i32.gt_s
                i32.add
                local.get 3
                i32.load8_s offset=3
                i32.const -65
                i32.gt_s
                i32.add
                local.set 4
                local.get 3
                i32.const 4
                i32.add
                local.set 3
                local.get 6
                i32.const 4
                i32.sub
                local.tee 6
                br_if 0 (;@6;)
              end
            end
            local.get 5
            i32.eqz
            br_if 0 (;@4;)
            loop  ;; label = @5
              local.get 4
              local.get 3
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.set 4
              local.get 3
              i32.const 1
              i32.add
              local.set 3
              local.get 5
              i32.const 1
              i32.sub
              local.tee 5
              br_if 0 (;@5;)
            end
          end
          local.get 4
          local.get 12
          i32.lt_u
          if  ;; label = @4
            local.get 12
            local.get 4
            i32.sub
            local.tee 4
            local.set 6
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 0
                  i32.load8_u offset=32
                  local.tee 3
                  i32.const 0
                  local.get 3
                  i32.const 3
                  i32.ne
                  select
                  i32.const 3
                  i32.and
                  local.tee 3
                  i32.const 1
                  i32.sub
                  br_table 0 (;@7;) 1 (;@6;) 2 (;@5;)
                end
                i32.const 0
                local.set 6
                local.get 4
                local.set 3
                br 1 (;@5;)
              end
              local.get 4
              i32.const 1
              i32.shr_u
              local.set 3
              local.get 4
              i32.const 1
              i32.add
              i32.const 1
              i32.shr_u
              local.set 6
            end
            local.get 3
            i32.const 1
            i32.add
            local.set 3
            local.get 0
            i32.const 4
            i32.add
            i32.load
            local.set 4
            local.get 0
            i32.load offset=28
            local.set 5
            local.get 0
            i32.load
            local.set 0
            block  ;; label = @5
              loop  ;; label = @6
                local.get 3
                i32.const 1
                i32.sub
                local.tee 3
                i32.eqz
                br_if 1 (;@5;)
                local.get 0
                local.get 5
                local.get 4
                i32.load offset=16
                call_indirect (type 0)
                i32.eqz
                br_if 0 (;@6;)
              end
              i32.const 1
              return
            end
            i32.const 1
            local.set 3
            local.get 5
            i32.const 1114112
            i32.eq
            br_if 2 (;@2;)
            local.get 0
            local.get 1
            local.get 2
            local.get 4
            i32.load offset=12
            call_indirect (type 2)
            br_if 2 (;@2;)
            i32.const 0
            local.set 3
            loop  ;; label = @5
              local.get 3
              local.get 6
              i32.eq
              if  ;; label = @6
                i32.const 0
                return
              end
              local.get 3
              i32.const 1
              i32.add
              local.set 3
              local.get 0
              local.get 5
              local.get 4
              i32.load offset=16
              call_indirect (type 0)
              i32.eqz
              br_if 0 (;@5;)
            end
            local.get 3
            i32.const 1
            i32.sub
            local.get 6
            i32.lt_u
            return
          end
          br 2 (;@1;)
        end
        local.get 0
        i32.load
        local.get 1
        local.get 2
        local.get 0
        i32.load offset=4
        i32.load offset=12
        call_indirect (type 2)
        local.set 3
      end
      local.get 3
      return
    end
    local.get 0
    i32.load
    local.get 1
    local.get 2
    local.get 0
    i32.load offset=4
    i32.load offset=12
    call_indirect (type 2))
  (func (;8;) (type 2) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    i32.const 3
    i32.store8 offset=40
    local.get 3
    i64.const 137438953472
    i64.store offset=32
    local.get 3
    i32.const 0
    i32.store offset=24
    local.get 3
    i32.const 0
    i32.store offset=16
    local.get 3
    local.get 1
    i32.store offset=12
    local.get 3
    local.get 0
    i32.store offset=8
    block (result i32)  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 2
          i32.load
          local.tee 10
          i32.eqz
          if  ;; label = @4
            local.get 2
            i32.const 20
            i32.add
            i32.load
            local.tee 0
            i32.eqz
            br_if 1 (;@3;)
            local.get 2
            i32.load offset=16
            local.set 1
            local.get 0
            i32.const 3
            i32.shl
            local.set 5
            local.get 0
            i32.const 1
            i32.sub
            i32.const 536870911
            i32.and
            i32.const 1
            i32.add
            local.set 7
            local.get 2
            i32.load offset=8
            local.set 0
            loop  ;; label = @5
              local.get 0
              i32.const 4
              i32.add
              i32.load
              local.tee 4
              if  ;; label = @6
                local.get 3
                i32.load offset=8
                local.get 0
                i32.load
                local.get 4
                local.get 3
                i32.load offset=12
                i32.load offset=12
                call_indirect (type 2)
                br_if 4 (;@2;)
              end
              local.get 1
              i32.load
              local.get 3
              i32.const 8
              i32.add
              local.get 1
              i32.const 4
              i32.add
              i32.load
              call_indirect (type 0)
              br_if 3 (;@2;)
              local.get 1
              i32.const 8
              i32.add
              local.set 1
              local.get 0
              i32.const 8
              i32.add
              local.set 0
              local.get 5
              i32.const 8
              i32.sub
              local.tee 5
              br_if 0 (;@5;)
            end
            br 1 (;@3;)
          end
          local.get 2
          i32.load offset=4
          local.tee 0
          i32.eqz
          br_if 0 (;@3;)
          local.get 0
          i32.const 5
          i32.shl
          local.set 11
          local.get 0
          i32.const 1
          i32.sub
          i32.const 134217727
          i32.and
          i32.const 1
          i32.add
          local.set 7
          local.get 2
          i32.load offset=8
          local.set 0
          loop  ;; label = @4
            local.get 0
            i32.const 4
            i32.add
            i32.load
            local.tee 1
            if  ;; label = @5
              local.get 3
              i32.load offset=8
              local.get 0
              i32.load
              local.get 1
              local.get 3
              i32.load offset=12
              i32.load offset=12
              call_indirect (type 2)
              br_if 3 (;@2;)
            end
            local.get 3
            local.get 5
            local.get 10
            i32.add
            local.tee 4
            i32.const 28
            i32.add
            i32.load8_u
            i32.store8 offset=40
            local.get 3
            local.get 4
            i32.const 20
            i32.add
            i64.load align=4
            i64.store offset=32
            local.get 4
            i32.const 16
            i32.add
            i32.load
            local.set 6
            local.get 2
            i32.load offset=16
            local.set 8
            i32.const 0
            local.set 9
            i32.const 0
            local.set 1
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 4
                  i32.const 12
                  i32.add
                  i32.load
                  i32.const 1
                  i32.sub
                  br_table 0 (;@7;) 2 (;@5;) 1 (;@6;)
                end
                local.get 6
                i32.const 3
                i32.shl
                local.get 8
                i32.add
                local.tee 12
                i32.const 4
                i32.add
                i32.load
                i32.const 42
                i32.ne
                br_if 1 (;@5;)
                local.get 12
                i32.load
                i32.load
                local.set 6
              end
              i32.const 1
              local.set 1
            end
            local.get 3
            local.get 6
            i32.store offset=20
            local.get 3
            local.get 1
            i32.store offset=16
            local.get 4
            i32.const 8
            i32.add
            i32.load
            local.set 1
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 4
                  i32.const 4
                  i32.add
                  i32.load
                  i32.const 1
                  i32.sub
                  br_table 0 (;@7;) 2 (;@5;) 1 (;@6;)
                end
                local.get 1
                i32.const 3
                i32.shl
                local.get 8
                i32.add
                local.tee 6
                i32.const 4
                i32.add
                i32.load
                i32.const 42
                i32.ne
                br_if 1 (;@5;)
                local.get 6
                i32.load
                i32.load
                local.set 1
              end
              i32.const 1
              local.set 9
            end
            local.get 3
            local.get 1
            i32.store offset=28
            local.get 3
            local.get 9
            i32.store offset=24
            local.get 8
            local.get 4
            i32.load
            i32.const 3
            i32.shl
            i32.add
            local.tee 1
            i32.load
            local.get 3
            i32.const 8
            i32.add
            local.get 1
            i32.load offset=4
            call_indirect (type 0)
            br_if 2 (;@2;)
            local.get 0
            i32.const 8
            i32.add
            local.set 0
            local.get 11
            local.get 5
            i32.const 32
            i32.add
            local.tee 5
            i32.ne
            br_if 0 (;@4;)
          end
        end
        local.get 2
        i32.const 12
        i32.add
        i32.load
        local.get 7
        i32.gt_u
        if  ;; label = @3
          local.get 3
          i32.load offset=8
          local.get 2
          i32.load offset=8
          local.get 7
          i32.const 3
          i32.shl
          i32.add
          local.tee 0
          i32.load
          local.get 0
          i32.load offset=4
          local.get 3
          i32.load offset=12
          i32.load offset=12
          call_indirect (type 2)
          br_if 1 (;@2;)
        end
        i32.const 0
        br 1 (;@1;)
      end
      i32.const 1
    end
    local.get 3
    i32.const 48
    i32.add
    global.set 0)
  (func (;9;) (type 1) (param i32 i32)
    (local i32 i32 i32 i32)
    local.get 0
    local.get 1
    call 94
    local.set 2
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          call 87
          br_if 0 (;@3;)
          local.get 0
          i32.load
          local.set 3
          block  ;; label = @4
            local.get 0
            call 78
            i32.eqz
            if  ;; label = @5
              local.get 1
              local.get 3
              i32.add
              local.set 1
              local.get 0
              local.get 3
              call 95
              local.tee 0
              i32.const 1050836
              i32.load
              i32.ne
              br_if 1 (;@4;)
              local.get 2
              i32.load offset=4
              i32.const 3
              i32.and
              i32.const 3
              i32.ne
              br_if 2 (;@3;)
              i32.const 1050828
              local.get 1
              i32.store
              local.get 0
              local.get 1
              local.get 2
              call 52
              return
            end
            local.get 1
            local.get 3
            i32.add
            i32.const 16
            i32.add
            local.set 0
            br 2 (;@2;)
          end
          local.get 3
          i32.const 256
          i32.ge_u
          if  ;; label = @4
            local.get 0
            call 14
            br 1 (;@3;)
          end
          local.get 0
          i32.const 12
          i32.add
          i32.load
          local.tee 4
          local.get 0
          i32.const 8
          i32.add
          i32.load
          local.tee 5
          i32.ne
          if  ;; label = @4
            local.get 5
            local.get 4
            i32.store offset=12
            local.get 4
            local.get 5
            i32.store offset=8
            br 1 (;@3;)
          end
          i32.const 1050820
          i32.const 1050820
          i32.load
          i32.const -2
          local.get 3
          i32.const 3
          i32.shr_u
          i32.rotl
          i32.and
          i32.store
        end
        local.get 2
        call 75
        if  ;; label = @3
          local.get 0
          local.get 1
          local.get 2
          call 52
          br 2 (;@1;)
        end
        block  ;; label = @3
          i32.const 1050840
          i32.load
          local.get 2
          i32.ne
          if  ;; label = @4
            local.get 2
            i32.const 1050836
            i32.load
            i32.ne
            br_if 1 (;@3;)
            i32.const 1050836
            local.get 0
            i32.store
            i32.const 1050828
            i32.const 1050828
            i32.load
            local.get 1
            i32.add
            local.tee 1
            i32.store
            local.get 0
            local.get 1
            call 65
            return
          end
          i32.const 1050840
          local.get 0
          i32.store
          i32.const 1050832
          i32.const 1050832
          i32.load
          local.get 1
          i32.add
          local.tee 1
          i32.store
          local.get 0
          local.get 1
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 0
          i32.const 1050836
          i32.load
          i32.ne
          br_if 1 (;@2;)
          i32.const 1050828
          i32.const 0
          i32.store
          i32.const 1050836
          i32.const 0
          i32.store
          return
        end
        local.get 2
        call 86
        local.tee 3
        local.get 1
        i32.add
        local.set 1
        block  ;; label = @3
          local.get 3
          i32.const 256
          i32.ge_u
          if  ;; label = @4
            local.get 2
            call 14
            br 1 (;@3;)
          end
          local.get 2
          i32.const 12
          i32.add
          i32.load
          local.tee 4
          local.get 2
          i32.const 8
          i32.add
          i32.load
          local.tee 2
          i32.ne
          if  ;; label = @4
            local.get 2
            local.get 4
            i32.store offset=12
            local.get 4
            local.get 2
            i32.store offset=8
            br 1 (;@3;)
          end
          i32.const 1050820
          i32.const 1050820
          i32.load
          i32.const -2
          local.get 3
          i32.const 3
          i32.shr_u
          i32.rotl
          i32.and
          i32.store
        end
        local.get 0
        local.get 1
        call 65
        local.get 0
        i32.const 1050836
        i32.load
        i32.ne
        br_if 1 (;@1;)
        i32.const 1050828
        local.get 1
        i32.store
      end
      return
    end
    local.get 1
    i32.const 256
    i32.ge_u
    if  ;; label = @1
      local.get 0
      local.get 1
      call 15
      return
    end
    local.get 1
    i32.const -8
    i32.and
    i32.const 1050556
    i32.add
    local.set 2
    block (result i32)  ;; label = @1
      i32.const 1050820
      i32.load
      local.tee 3
      i32.const 1
      local.get 1
      i32.const 3
      i32.shr_u
      i32.shl
      local.tee 1
      i32.and
      if  ;; label = @2
        local.get 2
        i32.load offset=8
        br 1 (;@1;)
      end
      i32.const 1050820
      local.get 1
      local.get 3
      i32.or
      i32.store
      local.get 2
    end
    local.set 1
    local.get 2
    local.get 0
    i32.store offset=8
    local.get 1
    local.get 0
    i32.store offset=12
    local.get 0
    local.get 2
    i32.store offset=12
    local.get 0
    local.get 1
    i32.store offset=8)
  (func (;10;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.const 9
            i32.ge_u
            if  ;; label = @5
              i32.const 16
              i32.const 8
              call 66
              local.get 1
              i32.gt_u
              br_if 1 (;@4;)
              br 2 (;@3;)
            end
            local.get 0
            call 5
            local.set 4
            br 2 (;@2;)
          end
          i32.const 16
          i32.const 8
          call 66
          local.set 1
        end
        i32.const 8
        i32.const 8
        call 66
        local.set 3
        i32.const 20
        i32.const 8
        call 66
        local.set 2
        i32.const 16
        i32.const 8
        call 66
        local.set 5
        i32.const 0
        i32.const 16
        i32.const 8
        call 66
        i32.const 2
        i32.shl
        i32.sub
        local.tee 6
        i32.const -65536
        local.get 5
        local.get 2
        local.get 3
        i32.add
        i32.add
        i32.sub
        i32.const -9
        i32.and
        i32.const 3
        i32.sub
        local.tee 3
        local.get 3
        local.get 6
        i32.gt_u
        select
        local.get 1
        i32.sub
        local.get 0
        i32.le_u
        br_if 0 (;@2;)
        local.get 1
        i32.const 16
        local.get 0
        i32.const 4
        i32.add
        i32.const 16
        i32.const 8
        call 66
        i32.const 5
        i32.sub
        local.get 0
        i32.gt_u
        select
        i32.const 8
        call 66
        local.tee 3
        i32.add
        i32.const 16
        i32.const 8
        call 66
        i32.add
        i32.const 4
        i32.sub
        call 5
        local.tee 2
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        call 97
        local.set 0
        block  ;; label = @3
          local.get 1
          i32.const 1
          i32.sub
          local.tee 4
          local.get 2
          i32.and
          i32.eqz
          if  ;; label = @4
            local.get 0
            local.set 1
            br 1 (;@3;)
          end
          local.get 2
          local.get 4
          i32.add
          i32.const 0
          local.get 1
          i32.sub
          i32.and
          call 97
          local.set 2
          i32.const 16
          i32.const 8
          call 66
          local.set 4
          local.get 0
          call 86
          local.get 2
          local.get 1
          i32.const 0
          local.get 2
          local.get 0
          i32.sub
          local.get 4
          i32.le_u
          select
          i32.add
          local.tee 1
          local.get 0
          i32.sub
          local.tee 2
          i32.sub
          local.set 4
          local.get 0
          call 78
          i32.eqz
          if  ;; label = @4
            local.get 1
            local.get 4
            call 48
            local.get 0
            local.get 2
            call 48
            local.get 0
            local.get 2
            call 9
            br 1 (;@3;)
          end
          local.get 0
          i32.load
          local.set 0
          local.get 1
          local.get 4
          i32.store offset=4
          local.get 1
          local.get 0
          local.get 2
          i32.add
          i32.store
        end
        local.get 1
        call 78
        br_if 1 (;@1;)
        local.get 1
        call 86
        local.tee 2
        i32.const 16
        i32.const 8
        call 66
        local.get 3
        i32.add
        i32.le_u
        br_if 1 (;@1;)
        local.get 1
        local.get 3
        call 94
        local.set 0
        local.get 1
        local.get 3
        call 48
        local.get 0
        local.get 2
        local.get 3
        i32.sub
        local.tee 3
        call 48
        local.get 0
        local.get 3
        call 9
        br 1 (;@1;)
      end
      local.get 4
      return
    end
    local.get 1
    call 96
    local.get 1
    call 78
    drop)
  (func (;11;) (type 0) (param i32 i32) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 2
    global.set 0
    local.get 0
    i32.load
    local.set 0
    block  ;; label = @1
      local.get 1
      i32.const 127
      i32.le_u
      if  ;; label = @2
        local.get 0
        i32.load offset=8
        local.tee 3
        local.get 0
        i32.load
        i32.eq
        if (result i32)  ;; label = @3
          local.get 0
          local.get 3
          call 20
          local.get 0
          i32.load offset=8
        else
          local.get 3
        end
        local.get 0
        i32.load offset=4
        i32.add
        local.get 1
        i32.store8
        local.get 0
        local.get 0
        i32.load offset=8
        i32.const 1
        i32.add
        i32.store offset=8
        br 1 (;@1;)
      end
      local.get 2
      i32.const 0
      i32.store offset=12
      block (result i32)  ;; label = @2
        local.get 1
        i32.const 2048
        i32.ge_u
        if  ;; label = @3
          local.get 1
          i32.const 65536
          i32.ge_u
          if  ;; label = @4
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=15
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=14
            local.get 2
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=13
            local.get 2
            local.get 1
            i32.const 18
            i32.shr_u
            i32.const 7
            i32.and
            i32.const 240
            i32.or
            i32.store8 offset=12
            i32.const 4
            br 2 (;@2;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=14
          local.get 2
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 224
          i32.or
          i32.store8 offset=12
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          i32.const 3
          br 1 (;@2;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 192
        i32.or
        i32.store8 offset=12
        i32.const 2
      end
      local.set 1
      local.get 1
      local.get 0
      i32.load
      local.get 0
      i32.load offset=8
      local.tee 3
      i32.sub
      i32.gt_u
      if  ;; label = @2
        local.get 0
        local.get 3
        local.get 1
        call 19
        local.get 0
        i32.load offset=8
        local.set 3
      end
      local.get 0
      i32.load offset=4
      local.get 3
      i32.add
      local.get 2
      i32.const 12
      i32.add
      local.get 1
      call 91
      drop
      local.get 0
      local.get 1
      local.get 3
      i32.add
      i32.store offset=8
    end
    local.get 2
    i32.const 16
    i32.add
    global.set 0
    i32.const 0)
  (func (;12;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 3
    global.set 0
    local.get 0
    i32.load
    local.set 0
    block  ;; label = @1
      local.get 1
      i32.const 127
      i32.le_u
      if  ;; label = @2
        local.get 0
        i32.load offset=8
        local.tee 2
        local.get 0
        i32.load
        i32.eq
        if  ;; label = @3
          global.get 0
          i32.const 32
          i32.sub
          local.tee 4
          global.set 0
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              i32.const 1
              i32.add
              local.tee 2
              i32.eqz
              br_if 0 (;@5;)
              i32.const 8
              local.get 0
              i32.load
              local.tee 5
              i32.const 1
              i32.shl
              local.tee 6
              local.get 2
              local.get 2
              local.get 6
              i32.lt_u
              select
              local.tee 2
              local.get 2
              i32.const 8
              i32.le_u
              select
              local.tee 2
              i32.const -1
              i32.xor
              i32.const 31
              i32.shr_u
              local.set 6
              block  ;; label = @6
                local.get 5
                if  ;; label = @7
                  local.get 4
                  i32.const 1
                  i32.store offset=24
                  local.get 4
                  local.get 5
                  i32.store offset=20
                  local.get 4
                  local.get 0
                  i32.const 4
                  i32.add
                  i32.load
                  i32.store offset=16
                  br 1 (;@6;)
                end
                local.get 4
                i32.const 0
                i32.store offset=24
              end
              local.get 4
              local.get 2
              local.get 6
              local.get 4
              i32.const 16
              i32.add
              call 25
              local.get 4
              i32.load offset=4
              local.set 5
              local.get 4
              i32.load
              i32.eqz
              if  ;; label = @6
                local.get 0
                local.get 2
                i32.store
                local.get 0
                local.get 5
                i32.store offset=4
                br 2 (;@4;)
              end
              local.get 4
              i32.const 8
              i32.add
              i32.load
              local.tee 2
              i32.const -2147483647
              i32.eq
              br_if 1 (;@4;)
              local.get 2
              i32.eqz
              br_if 0 (;@5;)
              local.get 5
              local.get 2
              call 90
              unreachable
            end
            call 40
            unreachable
          end
          local.get 4
          i32.const 32
          i32.add
          global.set 0
          local.get 0
          i32.load offset=8
          local.set 2
        end
        local.get 0
        local.get 2
        i32.const 1
        i32.add
        i32.store offset=8
        local.get 0
        i32.load offset=4
        local.get 2
        i32.add
        local.get 1
        i32.store8
        br 1 (;@1;)
      end
      local.get 3
      i32.const 0
      i32.store offset=12
      block (result i32)  ;; label = @2
        local.get 1
        i32.const 2048
        i32.ge_u
        if  ;; label = @3
          local.get 1
          i32.const 65536
          i32.ge_u
          if  ;; label = @4
            local.get 3
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=15
            local.get 3
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=14
            local.get 3
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=13
            local.get 3
            local.get 1
            i32.const 18
            i32.shr_u
            i32.const 7
            i32.and
            i32.const 240
            i32.or
            i32.store8 offset=12
            i32.const 4
            br 2 (;@2;)
          end
          local.get 3
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=14
          local.get 3
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 224
          i32.or
          i32.store8 offset=12
          local.get 3
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          i32.const 3
          br 1 (;@2;)
        end
        local.get 3
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 3
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 192
        i32.or
        i32.store8 offset=12
        i32.const 2
      end
      local.set 1
      local.get 1
      local.get 0
      i32.load
      local.get 0
      i32.load offset=8
      local.tee 2
      i32.sub
      i32.gt_u
      if  ;; label = @2
        local.get 0
        local.get 2
        local.get 1
        call 21
        local.get 0
        i32.load offset=8
        local.set 2
      end
      local.get 0
      i32.load offset=4
      local.get 2
      i32.add
      local.get 3
      i32.const 12
      i32.add
      local.get 1
      call 91
      drop
      local.get 0
      local.get 1
      local.get 2
      i32.add
      i32.store offset=8
    end
    local.get 3
    i32.const 16
    i32.add
    global.set 0
    i32.const 0)
  (func (;13;) (type 0) (param i32 i32) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 2
    global.set 0
    block  ;; label = @1
      local.get 1
      i32.const 127
      i32.le_u
      if  ;; label = @2
        local.get 0
        i32.load offset=8
        local.tee 3
        local.get 0
        i32.load
        i32.eq
        if (result i32)  ;; label = @3
          local.get 0
          local.get 3
          call 20
          local.get 0
          i32.load offset=8
        else
          local.get 3
        end
        local.get 0
        i32.load offset=4
        i32.add
        local.get 1
        i32.store8
        local.get 0
        local.get 0
        i32.load offset=8
        i32.const 1
        i32.add
        i32.store offset=8
        br 1 (;@1;)
      end
      local.get 2
      i32.const 0
      i32.store offset=12
      block (result i32)  ;; label = @2
        local.get 1
        i32.const 2048
        i32.ge_u
        if  ;; label = @3
          local.get 1
          i32.const 65536
          i32.ge_u
          if  ;; label = @4
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=15
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=14
            local.get 2
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=13
            local.get 2
            local.get 1
            i32.const 18
            i32.shr_u
            i32.const 7
            i32.and
            i32.const 240
            i32.or
            i32.store8 offset=12
            i32.const 4
            br 2 (;@2;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=14
          local.get 2
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 224
          i32.or
          i32.store8 offset=12
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          i32.const 3
          br 1 (;@2;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 192
        i32.or
        i32.store8 offset=12
        i32.const 2
      end
      local.set 1
      local.get 1
      local.get 0
      i32.load
      local.get 0
      i32.load offset=8
      local.tee 3
      i32.sub
      i32.gt_u
      if  ;; label = @2
        local.get 0
        local.get 3
        local.get 1
        call 19
        local.get 0
        i32.load offset=8
        local.set 3
      end
      local.get 0
      i32.load offset=4
      local.get 3
      i32.add
      local.get 2
      i32.const 12
      i32.add
      local.get 1
      call 91
      drop
      local.get 0
      local.get 1
      local.get 3
      i32.add
      i32.store offset=8
    end
    local.get 2
    i32.const 16
    i32.add
    global.set 0
    i32.const 0)
  (func (;14;) (type 4) (param i32)
    (local i32 i32 i32 i32 i32)
    local.get 0
    i32.load offset=24
    local.set 3
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        local.get 0
        i32.load offset=12
        i32.eq
        if  ;; label = @3
          local.get 0
          i32.const 20
          i32.const 16
          local.get 0
          i32.const 20
          i32.add
          local.tee 1
          i32.load
          local.tee 4
          select
          i32.add
          i32.load
          local.tee 2
          br_if 1 (;@2;)
          i32.const 0
          local.set 1
          br 2 (;@1;)
        end
        local.get 0
        i32.load offset=8
        local.tee 2
        local.get 0
        i32.load offset=12
        local.tee 1
        i32.store offset=12
        local.get 1
        local.get 2
        i32.store offset=8
        br 1 (;@1;)
      end
      local.get 1
      local.get 0
      i32.const 16
      i32.add
      local.get 4
      select
      local.set 4
      loop  ;; label = @2
        local.get 4
        local.set 5
        local.get 2
        local.tee 1
        i32.const 20
        i32.add
        local.tee 2
        local.get 1
        i32.const 16
        i32.add
        local.get 2
        i32.load
        local.tee 2
        select
        local.set 4
        local.get 1
        i32.const 20
        i32.const 16
        local.get 2
        select
        i32.add
        i32.load
        local.tee 2
        br_if 0 (;@2;)
      end
      local.get 5
      i32.const 0
      i32.store
    end
    block  ;; label = @1
      local.get 3
      i32.eqz
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 0
        local.get 0
        i32.load offset=28
        i32.const 2
        i32.shl
        i32.const 1050412
        i32.add
        local.tee 2
        i32.load
        i32.ne
        if  ;; label = @3
          local.get 3
          i32.const 16
          i32.const 20
          local.get 3
          i32.load offset=16
          local.get 0
          i32.eq
          select
          i32.add
          local.get 1
          i32.store
          local.get 1
          i32.eqz
          br_if 2 (;@1;)
          br 1 (;@2;)
        end
        local.get 2
        local.get 1
        i32.store
        local.get 1
        br_if 0 (;@2;)
        i32.const 1050824
        i32.const 1050824
        i32.load
        i32.const -2
        local.get 0
        i32.load offset=28
        i32.rotl
        i32.and
        i32.store
        return
      end
      local.get 1
      local.get 3
      i32.store offset=24
      local.get 0
      i32.load offset=16
      local.tee 2
      if  ;; label = @2
        local.get 1
        local.get 2
        i32.store offset=16
        local.get 2
        local.get 1
        i32.store offset=24
      end
      local.get 0
      i32.const 20
      i32.add
      i32.load
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      i32.const 20
      i32.add
      local.get 0
      i32.store
      local.get 0
      local.get 1
      i32.store offset=24
    end)
  (func (;15;) (type 1) (param i32 i32)
    (local i32 i32 i32 i32)
    local.get 0
    i64.const 0
    i64.store offset=16 align=4
    local.get 0
    block (result i32)  ;; label = @1
      i32.const 0
      local.get 1
      i32.const 256
      i32.lt_u
      br_if 0 (;@1;)
      drop
      i32.const 31
      local.get 1
      i32.const 16777215
      i32.gt_u
      br_if 0 (;@1;)
      drop
      local.get 1
      i32.const 6
      local.get 1
      i32.const 8
      i32.shr_u
      i32.clz
      local.tee 2
      i32.sub
      i32.shr_u
      i32.const 1
      i32.and
      local.get 2
      i32.const 1
      i32.shl
      i32.sub
      i32.const 62
      i32.add
    end
    local.tee 3
    i32.store offset=28
    local.get 3
    i32.const 2
    i32.shl
    i32.const 1050412
    i32.add
    local.set 2
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            i32.const 1050824
            i32.load
            local.tee 4
            i32.const 1
            local.get 3
            i32.shl
            local.tee 5
            i32.and
            if  ;; label = @5
              local.get 2
              i32.load
              local.set 2
              local.get 3
              call 64
              local.set 3
              local.get 2
              call 86
              local.get 1
              i32.ne
              br_if 1 (;@4;)
              local.get 2
              local.set 3
              br 2 (;@3;)
            end
            i32.const 1050824
            local.get 4
            local.get 5
            i32.or
            i32.store
            local.get 2
            local.get 0
            i32.store
            br 3 (;@1;)
          end
          local.get 1
          local.get 3
          i32.shl
          local.set 4
          loop  ;; label = @4
            local.get 2
            local.get 4
            i32.const 29
            i32.shr_u
            i32.const 4
            i32.and
            i32.add
            i32.const 16
            i32.add
            local.tee 5
            i32.load
            local.tee 3
            i32.eqz
            br_if 2 (;@2;)
            local.get 4
            i32.const 1
            i32.shl
            local.set 4
            local.get 3
            local.tee 2
            call 86
            local.get 1
            i32.ne
            br_if 0 (;@4;)
          end
        end
        local.get 3
        i32.load offset=8
        local.tee 1
        local.get 0
        i32.store offset=12
        local.get 3
        local.get 0
        i32.store offset=8
        local.get 0
        local.get 3
        i32.store offset=12
        local.get 0
        local.get 1
        i32.store offset=8
        local.get 0
        i32.const 0
        i32.store offset=24
        return
      end
      local.get 5
      local.get 0
      i32.store
    end
    local.get 0
    local.get 2
    i32.store offset=24
    local.get 0
    local.get 0
    i32.store offset=8
    local.get 0
    local.get 0
    i32.store offset=12)
  (func (;16;) (type 6) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    i32.const 1050548
    i32.load
    local.tee 2
    if  ;; label = @1
      i32.const 1050540
      local.set 6
      loop  ;; label = @2
        local.get 2
        local.tee 1
        i32.load offset=8
        local.set 2
        local.get 1
        i32.load offset=4
        local.set 3
        local.get 1
        i32.load
        local.set 4
        local.get 1
        i32.const 12
        i32.add
        i32.load
        drop
        local.get 1
        local.set 6
        local.get 5
        i32.const 1
        i32.add
        local.set 5
        local.get 2
        br_if 0 (;@2;)
      end
    end
    i32.const 1050860
    i32.const 4095
    local.get 5
    local.get 5
    i32.const 4095
    i32.le_u
    select
    i32.store
    local.get 8)
  (func (;17;) (type 1) (param i32 i32)
    (local i32 i32 i32 i64)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 2
    global.set 0
    local.get 1
    i32.load offset=4
    i32.eqz
    if  ;; label = @1
      local.get 1
      i32.load offset=12
      local.set 3
      local.get 2
      i32.const 16
      i32.add
      local.tee 4
      i32.const 0
      i32.store
      local.get 2
      i64.const 4294967296
      i64.store offset=8
      local.get 2
      local.get 2
      i32.const 8
      i32.add
      i32.store offset=20
      local.get 2
      i32.const 40
      i32.add
      local.get 3
      i32.const 16
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      i32.const 32
      i32.add
      local.get 3
      i32.const 8
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      local.get 3
      i64.load align=4
      i64.store offset=24
      local.get 2
      i32.const 20
      i32.add
      i32.const 1049524
      local.get 2
      i32.const 24
      i32.add
      call 8
      drop
      local.get 1
      i32.const 8
      i32.add
      local.get 4
      i32.load
      i32.store
      local.get 1
      local.get 2
      i64.load offset=8
      i64.store align=4
    end
    local.get 1
    i64.load align=4
    local.set 5
    local.get 1
    i64.const 4294967296
    i64.store align=4
    local.get 2
    i32.const 32
    i32.add
    local.tee 3
    local.get 1
    i32.const 8
    i32.add
    local.tee 1
    i32.load
    i32.store
    local.get 1
    i32.const 0
    i32.store
    local.get 2
    local.get 5
    i64.store offset=24
    i32.const 12
    i32.const 4
    call 73
    local.tee 1
    i32.eqz
    if  ;; label = @1
      i32.const 12
      i32.const 4
      call 90
      unreachable
    end
    local.get 1
    local.get 2
    i64.load offset=24
    i64.store align=4
    local.get 1
    i32.const 8
    i32.add
    local.get 3
    i32.load
    i32.store
    local.get 0
    i32.const 1049820
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store
    local.get 2
    i32.const 48
    i32.add
    global.set 0)
  (func (;18;) (type 6) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 4
    global.set 0
    i32.const 1050353
    i32.load8_u
    i32.const 3
    i32.ne
    if  ;; label = @1
      local.get 4
      i32.const 1
      i32.store8 offset=11
      local.get 4
      local.get 4
      i32.const 11
      i32.add
      i32.store offset=12
      local.get 4
      i32.const 12
      i32.add
      local.set 1
      global.get 0
      i32.const 32
      i32.sub
      local.tee 0
      global.set 0
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    i32.const 1050353
                    i32.load8_u
                    i32.const 1
                    i32.sub
                    br_table 2 (;@6;) 4 (;@4;) 1 (;@7;) 0 (;@8;)
                  end
                  i32.const 1050353
                  i32.const 2
                  i32.store8
                  local.get 0
                  i32.const 1050353
                  i32.store offset=8
                  local.get 1
                  i32.load
                  local.tee 1
                  i32.load8_u
                  local.get 1
                  i32.const 0
                  i32.store8
                  i32.const 1
                  i32.and
                  i32.eqz
                  br_if 2 (;@5;)
                  global.get 0
                  i32.const 32
                  i32.sub
                  local.tee 1
                  global.set 0
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        i32.const 1050408
                        i32.load
                        i32.const 2147483647
                        i32.and
                        if  ;; label = @11
                          call 93
                          i32.eqz
                          br_if 1 (;@10;)
                        end
                        i32.const 1050392
                        i32.load
                        i32.const 1050392
                        i32.const -1
                        i32.store
                        br_if 1 (;@9;)
                        block  ;; label = @11
                          block  ;; label = @12
                            i32.const 1050408
                            i32.load
                            i32.const 2147483647
                            i32.and
                            i32.eqz
                            if  ;; label = @13
                              i32.const 1050404
                              i32.load
                              local.set 2
                              i32.const 1050404
                              i32.const 1048868
                              i32.store
                              i32.const 1050400
                              i32.load
                              local.set 3
                              i32.const 1050400
                              i32.const 1
                              i32.store
                              br 1 (;@12;)
                            end
                            call 93
                            i32.const 1050404
                            i32.load
                            local.set 2
                            i32.const 1050404
                            i32.const 1048868
                            i32.store
                            i32.const 1050400
                            i32.load
                            local.set 3
                            i32.const 1050400
                            i32.const 1
                            i32.store
                            i32.eqz
                            br_if 1 (;@11;)
                          end
                          i32.const 1050408
                          i32.load
                          i32.const 2147483647
                          i32.and
                          i32.eqz
                          br_if 0 (;@11;)
                          call 93
                          br_if 0 (;@11;)
                          i32.const 1050396
                          i32.const 1
                          i32.store8
                        end
                        i32.const 1050392
                        i32.const 0
                        i32.store
                        block  ;; label = @11
                          local.get 3
                          i32.eqz
                          br_if 0 (;@11;)
                          local.get 3
                          local.get 2
                          i32.load
                          call_indirect (type 4)
                          local.get 2
                          i32.const 4
                          i32.add
                          i32.load
                          i32.eqz
                          br_if 0 (;@11;)
                          local.get 2
                          i32.const 8
                          i32.add
                          i32.load
                          drop
                          local.get 3
                          call 6
                        end
                        local.get 1
                        i32.const 32
                        i32.add
                        global.set 0
                        br 2 (;@8;)
                      end
                      local.get 1
                      i32.const 20
                      i32.add
                      i32.const 1
                      i32.store
                      local.get 1
                      i32.const 28
                      i32.add
                      i32.const 0
                      i32.store
                      local.get 1
                      i32.const 1049736
                      i32.store offset=16
                      local.get 1
                      i32.const 1049548
                      i32.store offset=24
                      local.get 1
                      i32.const 0
                      i32.store offset=8
                      local.get 1
                      i32.const 8
                      i32.add
                      i32.const 1049772
                      call 43
                      unreachable
                    end
                    unreachable
                  end
                  local.get 0
                  i32.const 3
                  i32.store8 offset=12
                  local.get 0
                  i32.const 8
                  i32.add
                  local.tee 1
                  i32.load
                  local.get 1
                  i32.load8_u offset=4
                  i32.store8
                end
                local.get 0
                i32.const 32
                i32.add
                global.set 0
                br 4 (;@2;)
              end
              local.get 0
              i32.const 20
              i32.add
              i32.const 1
              i32.store
              local.get 0
              i32.const 28
              i32.add
              i32.const 0
              i32.store
              local.get 0
              i32.const 1049000
              i32.store offset=16
              br 2 (;@3;)
            end
            i32.const 1049008
            i32.const 1049128
            call 35
            unreachable
          end
          local.get 0
          i32.const 20
          i32.add
          i32.const 1
          i32.store
          local.get 0
          i32.const 28
          i32.add
          i32.const 0
          i32.store
          local.get 0
          i32.const 1048948
          i32.store offset=16
        end
        local.get 0
        i32.const 1048956
        i32.store offset=24
        local.get 0
        i32.const 0
        i32.store offset=8
        local.get 0
        i32.const 8
        i32.add
        i32.const 1048836
        call 43
        unreachable
      end
    end
    block  ;; label = @1
      i32.const 16384
      i32.const 1
      call 73
      local.tee 2
      if  ;; label = @2
        i32.const -1
        local.set 0
        i32.const 1
        local.set 1
        loop  ;; label = @3
          local.get 2
          local.get 5
          i32.add
          local.tee 3
          i32.const 1
          i32.store8
          local.get 3
          i32.const 1
          i32.add
          local.get 0
          local.get 1
          i32.const 7
          i32.div_u
          i32.const 7
          i32.mul
          i32.add
          i32.eqz
          i32.store8
          local.get 1
          i32.const 2
          i32.add
          local.set 1
          local.get 0
          i32.const 2
          i32.sub
          local.set 0
          local.get 5
          i32.const 2
          i32.add
          local.tee 5
          i32.const 16384
          i32.ne
          br_if 0 (;@3;)
        end
        i32.const 24
        i32.const 4
        call 73
        local.tee 0
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.const 16384
        i32.store offset=20
        local.get 0
        local.get 2
        i32.store offset=16
        local.get 0
        i64.const 70368744177792
        i64.store offset=8 align=4
        local.get 0
        i64.const 549755813888
        i64.store align=4
        local.get 4
        i32.const 16
        i32.add
        global.set 0
        local.get 0
        return
      end
      i32.const 16384
      i32.const 1
      call 90
      unreachable
    end
    i32.const 24
    i32.const 4
    call 90
    unreachable)
  (func (;19;) (type 5) (param i32 i32 i32)
    (local i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 3
    global.set 0
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        local.get 1
        local.get 2
        i32.add
        local.tee 1
        i32.gt_u
        br_if 0 (;@2;)
        i32.const 8
        local.get 0
        i32.load
        local.tee 2
        i32.const 1
        i32.shl
        local.tee 4
        local.get 1
        local.get 1
        local.get 4
        i32.lt_u
        select
        local.tee 1
        local.get 1
        i32.const 8
        i32.le_u
        select
        local.tee 1
        i32.const -1
        i32.xor
        i32.const 31
        i32.shr_u
        local.set 4
        block  ;; label = @3
          local.get 2
          if  ;; label = @4
            local.get 3
            i32.const 1
            i32.store offset=24
            local.get 3
            local.get 2
            i32.store offset=20
            local.get 3
            local.get 0
            i32.const 4
            i32.add
            i32.load
            i32.store offset=16
            br 1 (;@3;)
          end
          local.get 3
          i32.const 0
          i32.store offset=24
        end
        local.get 3
        local.get 1
        local.get 4
        local.get 3
        i32.const 16
        i32.add
        call 25
        local.get 3
        i32.load offset=4
        local.set 2
        local.get 3
        i32.load
        i32.eqz
        if  ;; label = @3
          local.get 0
          local.get 1
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.get 2
          i32.store
          br 2 (;@1;)
        end
        local.get 3
        i32.const 8
        i32.add
        i32.load
        local.tee 0
        i32.const -2147483647
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        local.get 0
        call 90
        unreachable
      end
      call 40
      unreachable
    end
    local.get 3
    i32.const 32
    i32.add
    global.set 0)
  (func (;20;) (type 1) (param i32 i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 1
        i32.add
        local.tee 1
        i32.eqz
        br_if 0 (;@2;)
        i32.const 8
        local.get 0
        i32.load
        local.tee 3
        i32.const 1
        i32.shl
        local.tee 4
        local.get 1
        local.get 1
        local.get 4
        i32.lt_u
        select
        local.tee 1
        local.get 1
        i32.const 8
        i32.le_u
        select
        local.tee 1
        i32.const -1
        i32.xor
        i32.const 31
        i32.shr_u
        local.set 4
        block  ;; label = @3
          local.get 3
          if  ;; label = @4
            local.get 2
            i32.const 1
            i32.store offset=24
            local.get 2
            local.get 3
            i32.store offset=20
            local.get 2
            local.get 0
            i32.const 4
            i32.add
            i32.load
            i32.store offset=16
            br 1 (;@3;)
          end
          local.get 2
          i32.const 0
          i32.store offset=24
        end
        local.get 2
        local.get 1
        local.get 4
        local.get 2
        i32.const 16
        i32.add
        call 25
        local.get 2
        i32.load offset=4
        local.set 3
        local.get 2
        i32.load
        i32.eqz
        if  ;; label = @3
          local.get 0
          local.get 1
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.get 3
          i32.store
          br 2 (;@1;)
        end
        local.get 2
        i32.const 8
        i32.add
        i32.load
        local.tee 0
        i32.const -2147483647
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        local.get 0
        call 90
        unreachable
      end
      call 40
      unreachable
    end
    local.get 2
    i32.const 32
    i32.add
    global.set 0)
  (func (;21;) (type 5) (param i32 i32 i32)
    (local i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 3
    global.set 0
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        local.get 1
        local.get 2
        i32.add
        local.tee 1
        i32.gt_u
        br_if 0 (;@2;)
        i32.const 8
        local.get 0
        i32.load
        local.tee 2
        i32.const 1
        i32.shl
        local.tee 4
        local.get 1
        local.get 1
        local.get 4
        i32.lt_u
        select
        local.tee 1
        local.get 1
        i32.const 8
        i32.le_u
        select
        local.tee 1
        i32.const -1
        i32.xor
        i32.const 31
        i32.shr_u
        local.set 4
        block  ;; label = @3
          local.get 2
          if  ;; label = @4
            local.get 3
            i32.const 1
            i32.store offset=24
            local.get 3
            local.get 2
            i32.store offset=20
            local.get 3
            local.get 0
            i32.const 4
            i32.add
            i32.load
            i32.store offset=16
            br 1 (;@3;)
          end
          local.get 3
          i32.const 0
          i32.store offset=24
        end
        local.get 3
        local.get 1
        local.get 4
        local.get 3
        i32.const 16
        i32.add
        call 25
        local.get 3
        i32.load offset=4
        local.set 2
        local.get 3
        i32.load
        i32.eqz
        if  ;; label = @3
          local.get 0
          local.get 1
          i32.store
          local.get 0
          local.get 2
          i32.store offset=4
          br 2 (;@1;)
        end
        local.get 3
        i32.const 8
        i32.add
        i32.load
        local.tee 0
        i32.const -2147483647
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        local.get 0
        call 90
        unreachable
      end
      call 40
      unreachable
    end
    local.get 3
    i32.const 32
    i32.add
    global.set 0)
  (func (;22;) (type 8) (param i32 i32 i32 i32 i32)
    (local i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 5
    global.set 0
    i32.const 1050408
    i32.const 1050408
    i32.load
    local.tee 6
    i32.const 1
    i32.add
    i32.store
    block  ;; label = @1
      block  ;; label = @2
        local.get 6
        i32.const 0
        i32.lt_s
        br_if 0 (;@2;)
        i32.const 1050864
        i32.const 1050864
        i32.load
        i32.const 1
        i32.add
        local.tee 6
        i32.store
        local.get 6
        i32.const 2
        i32.gt_u
        br_if 0 (;@2;)
        local.get 5
        local.get 4
        i32.store8 offset=24
        local.get 5
        local.get 3
        i32.store offset=20
        local.get 5
        local.get 2
        i32.store offset=16
        local.get 5
        i32.const 1049892
        i32.store offset=12
        local.get 5
        i32.const 1049548
        i32.store offset=8
        i32.const 1050392
        i32.load
        local.tee 2
        i32.const 0
        i32.lt_s
        br_if 0 (;@2;)
        i32.const 1050392
        local.get 2
        i32.const 1
        i32.add
        local.tee 2
        i32.store
        i32.const 1050392
        i32.const 1050400
        i32.load
        if (result i32)  ;; label = @3
          local.get 5
          local.get 0
          local.get 1
          i32.load offset=16
          call_indirect (type 1)
          local.get 5
          local.get 5
          i64.load
          i64.store offset=8
          i32.const 1050400
          i32.load
          local.get 5
          i32.const 8
          i32.add
          i32.const 1050404
          i32.load
          i32.load offset=20
          call_indirect (type 1)
          i32.const 1050392
          i32.load
        else
          local.get 2
        end
        i32.const 1
        i32.sub
        i32.store
        local.get 6
        i32.const 1
        i32.gt_u
        br_if 0 (;@2;)
        local.get 4
        br_if 1 (;@1;)
      end
      unreachable
    end
    global.get 0
    i32.const 16
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    local.get 1
    i32.store offset=12
    local.get 2
    local.get 0
    i32.store offset=8
    unreachable)
  (func (;23;) (type 1) (param i32 i32)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          if  ;; label = @4
            local.get 0
            i32.load
            br_if 1 (;@3;)
            local.get 0
            local.get 1
            i32.store offset=4
            local.get 0
            i32.const -1
            i32.store
            block  ;; label = @5
              local.get 0
              i32.const 8
              i32.add
              i32.load
              local.get 1
              i32.mul
              local.tee 1
              i32.eqz
              if  ;; label = @6
                i32.const 1
                local.set 2
                br 1 (;@5;)
              end
              local.get 1
              i32.const 0
              i32.lt_s
              br_if 3 (;@2;)
              local.get 1
              i32.const 1
              call 73
              local.tee 2
              i32.eqz
              br_if 4 (;@1;)
              local.get 2
              local.get 1
              call 92
            end
            local.get 0
            i32.const 12
            i32.add
            i32.load
            if  ;; label = @5
              local.get 0
              i32.const 16
              i32.add
              i32.load
              call 6
            end
            local.get 0
            local.get 1
            i32.store offset=12
            local.get 0
            i32.const 0
            i32.store
            local.get 0
            i32.const 20
            i32.add
            local.get 1
            i32.store
            local.get 0
            i32.const 16
            i32.add
            local.get 2
            i32.store
            return
          end
          call 83
          unreachable
        end
        call 84
        unreachable
      end
      call 40
      unreachable
    end
    local.get 1
    i32.const 1
    call 90
    unreachable)
  (func (;24;) (type 1) (param i32 i32)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          if  ;; label = @4
            local.get 0
            i32.load
            br_if 1 (;@3;)
            local.get 0
            i32.const -1
            i32.store
            local.get 0
            i32.const 8
            i32.add
            local.get 1
            i32.store
            block  ;; label = @5
              local.get 0
              i32.load offset=4
              local.get 1
              i32.mul
              local.tee 1
              i32.eqz
              if  ;; label = @6
                i32.const 1
                local.set 2
                br 1 (;@5;)
              end
              local.get 1
              i32.const 0
              i32.lt_s
              br_if 3 (;@2;)
              local.get 1
              i32.const 1
              call 73
              local.tee 2
              i32.eqz
              br_if 4 (;@1;)
              local.get 2
              local.get 1
              call 92
            end
            local.get 0
            i32.const 12
            i32.add
            i32.load
            if  ;; label = @5
              local.get 0
              i32.const 16
              i32.add
              i32.load
              call 6
            end
            local.get 0
            local.get 1
            i32.store offset=12
            local.get 0
            i32.const 0
            i32.store
            local.get 0
            i32.const 20
            i32.add
            local.get 1
            i32.store
            local.get 0
            i32.const 16
            i32.add
            local.get 2
            i32.store
            return
          end
          call 83
          unreachable
        end
        call 84
        unreachable
      end
      call 40
      unreachable
    end
    local.get 1
    i32.const 1
    call 90
    unreachable)
  (func (;25;) (type 9) (param i32 i32 i32 i32)
    (local i32)
    block  ;; label = @1
      local.get 2
      if  ;; label = @2
        block (result i32)  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 1
                i32.const 0
                i32.ge_s
                if  ;; label = @7
                  local.get 3
                  i32.load offset=8
                  i32.eqz
                  br_if 2 (;@5;)
                  local.get 3
                  i32.load offset=4
                  local.tee 4
                  br_if 1 (;@6;)
                  local.get 1
                  br_if 3 (;@4;)
                  local.get 2
                  br 4 (;@3;)
                end
                local.get 0
                i32.const 8
                i32.add
                i32.const 0
                i32.store
                br 5 (;@1;)
              end
              local.get 3
              i32.load
              local.get 4
              local.get 2
              local.get 1
              call 67
              br 2 (;@3;)
            end
            local.get 1
            br_if 0 (;@4;)
            local.get 2
            br 1 (;@3;)
          end
          local.get 1
          local.get 2
          call 73
        end
        local.tee 3
        if  ;; label = @3
          local.get 0
          local.get 3
          i32.store offset=4
          local.get 0
          i32.const 8
          i32.add
          local.get 1
          i32.store
          local.get 0
          i32.const 0
          i32.store
          return
        end
        local.get 0
        local.get 1
        i32.store offset=4
        local.get 0
        i32.const 8
        i32.add
        local.get 2
        i32.store
        br 1 (;@1;)
      end
      local.get 0
      local.get 1
      i32.store offset=4
      local.get 0
      i32.const 8
      i32.add
      i32.const 0
      i32.store
    end
    local.get 0
    i32.const 1
    i32.store)
  (func (;26;) (type 1) (param i32 i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 2
    global.set 0
    local.get 1
    i32.load offset=4
    i32.eqz
    if  ;; label = @1
      local.get 1
      i32.load offset=12
      local.set 3
      local.get 2
      i32.const 16
      i32.add
      local.tee 4
      i32.const 0
      i32.store
      local.get 2
      i64.const 4294967296
      i64.store offset=8
      local.get 2
      local.get 2
      i32.const 8
      i32.add
      i32.store offset=20
      local.get 2
      i32.const 40
      i32.add
      local.get 3
      i32.const 16
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      i32.const 32
      i32.add
      local.get 3
      i32.const 8
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      local.get 3
      i64.load align=4
      i64.store offset=24
      local.get 2
      i32.const 20
      i32.add
      i32.const 1049524
      local.get 2
      i32.const 24
      i32.add
      call 8
      drop
      local.get 1
      i32.const 8
      i32.add
      local.get 4
      i32.load
      i32.store
      local.get 1
      local.get 2
      i64.load offset=8
      i64.store align=4
    end
    local.get 0
    i32.const 1049820
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store
    local.get 2
    i32.const 48
    i32.add
    global.set 0)
  (func (;27;) (type 13) (param i32 i32 i32 i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 6
    global.set 0
    block  ;; label = @1
      local.get 1
      if  ;; label = @2
        local.get 6
        local.get 1
        local.get 3
        local.get 4
        local.get 5
        local.get 2
        i32.load offset=16
        call_indirect (type 8)
        local.get 6
        i32.load offset=4
        local.set 1
        block  ;; label = @3
          local.get 6
          i32.load
          local.tee 3
          local.get 6
          i32.load offset=8
          local.tee 2
          i32.le_u
          if  ;; label = @4
            local.get 1
            local.set 4
            br 1 (;@3;)
          end
          local.get 2
          i32.eqz
          if  ;; label = @4
            i32.const 4
            local.set 4
            local.get 1
            call 6
            br 1 (;@3;)
          end
          local.get 1
          local.get 3
          i32.const 2
          i32.shl
          i32.const 4
          local.get 2
          i32.const 2
          i32.shl
          local.tee 1
          call 67
          local.tee 4
          i32.eqz
          br_if 2 (;@1;)
        end
        local.get 0
        local.get 2
        i32.store offset=4
        local.get 0
        local.get 4
        i32.store
        local.get 6
        i32.const 16
        i32.add
        global.set 0
        return
      end
      i32.const 1049366
      i32.const 50
      call 85
      unreachable
    end
    local.get 1
    i32.const 4
    call 90
    unreachable)
  (func (;28;) (type 5) (param i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    local.get 1
    i32.store offset=4
    local.get 3
    local.get 0
    i32.store
    local.get 3
    i32.const 20
    i32.add
    i32.const 2
    i32.store
    local.get 3
    i32.const 28
    i32.add
    i32.const 2
    i32.store
    local.get 3
    i32.const 44
    i32.add
    i32.const 27
    i32.store
    local.get 3
    i32.const 1050032
    i32.store offset=16
    local.get 3
    i32.const 0
    i32.store offset=8
    local.get 3
    i32.const 27
    i32.store offset=36
    local.get 3
    local.get 3
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 3
    local.get 3
    i32.store offset=40
    local.get 3
    local.get 3
    i32.const 4
    i32.add
    i32.store offset=32
    local.get 3
    i32.const 8
    i32.add
    local.get 2
    call 43
    unreachable)
  (func (;29;) (type 5) (param i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        if  ;; label = @3
          local.get 0
          i32.load
          br_if 1 (;@2;)
          local.get 0
          i32.const -1
          i32.store
          local.get 0
          i32.const 4
          i32.add
          i32.load
          local.get 1
          i32.mul
          local.get 2
          i32.add
          local.tee 1
          local.get 0
          i32.const 20
          i32.add
          i32.load
          local.tee 2
          i32.ge_u
          br_if 2 (;@1;)
          local.get 0
          i32.const 16
          i32.add
          i32.load
          local.get 1
          i32.add
          local.tee 1
          local.get 1
          i32.load8_u
          i32.const 1
          i32.xor
          i32.store8
          local.get 0
          i32.const 0
          i32.store
          return
        end
        call 83
        unreachable
      end
      call 84
      unreachable
    end
    local.get 1
    local.get 2
    i32.const 1048852
    call 28
    unreachable)
  (func (;30;) (type 1) (param i32 i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 1
    global.set 0
    i32.const 1050352
    i32.load8_u
    if  ;; label = @1
      local.get 1
      i32.const 20
      i32.add
      i32.const 2
      i32.store
      local.get 1
      i32.const 28
      i32.add
      i32.const 1
      i32.store
      local.get 1
      i32.const 1049628
      i32.store offset=16
      local.get 1
      i32.const 0
      i32.store offset=8
      local.get 1
      i32.const 27
      i32.store offset=36
      local.get 1
      local.get 0
      i32.store offset=44
      local.get 1
      local.get 1
      i32.const 32
      i32.add
      i32.store offset=24
      local.get 1
      local.get 1
      i32.const 44
      i32.add
      i32.store offset=32
      local.get 1
      i32.const 8
      i32.add
      i32.const 1049668
      call 43
      unreachable
    end
    local.get 1
    i32.const 48
    i32.add
    global.set 0)
  (func (;31;) (type 0) (param i32 i32) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 1
    i32.load offset=4
    local.set 3
    local.get 1
    i32.load
    local.get 2
    i32.const 24
    i32.add
    local.get 0
    i32.load
    local.tee 0
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 16
    i32.add
    local.get 0
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 0
    i64.load align=4
    i64.store offset=8
    local.get 3
    local.get 2
    i32.const 8
    i32.add
    call 8
    local.get 2
    i32.const 32
    i32.add
    global.set 0)
  (func (;32;) (type 0) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    local.get 0
    i32.load
    i32.store offset=4
    local.get 2
    i32.const 24
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 16
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1049144
    local.get 2
    i32.const 8
    i32.add
    call 8
    local.get 2
    i32.const 32
    i32.add
    global.set 0)
  (func (;33;) (type 0) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    local.get 0
    i32.load
    i32.store offset=4
    local.get 2
    i32.const 24
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 16
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1049524
    local.get 2
    i32.const 8
    i32.add
    call 8
    local.get 2
    i32.const 32
    i32.add
    global.set 0)
  (func (;34;) (type 0) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    local.get 0
    i32.store offset=4
    local.get 2
    i32.const 24
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 16
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1049144
    local.get 2
    i32.const 8
    i32.add
    call 8
    local.get 2
    i32.const 32
    i32.add
    global.set 0)
  (func (;35;) (type 1) (param i32 i32)
    (local i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    i32.const 12
    i32.add
    i32.const 1
    i32.store
    local.get 2
    i32.const 20
    i32.add
    i32.const 0
    i32.store
    local.get 2
    i32.const 1049980
    i32.store offset=16
    local.get 2
    i32.const 0
    i32.store
    local.get 2
    i32.const 43
    i32.store offset=28
    local.get 2
    local.get 0
    i32.store offset=24
    local.get 2
    local.get 2
    i32.const 24
    i32.add
    i32.store offset=8
    local.get 2
    local.get 1
    call 43
    unreachable)
  (func (;36;) (type 2) (param i32 i32 i32) (result i32)
    (local i32)
    local.get 2
    local.get 0
    i32.load
    local.tee 0
    i32.load
    local.get 0
    i32.load offset=8
    local.tee 3
    i32.sub
    i32.gt_u
    if  ;; label = @1
      local.get 0
      local.get 3
      local.get 2
      call 19
      local.get 0
      i32.load offset=8
      local.set 3
    end
    local.get 0
    i32.load offset=4
    local.get 3
    i32.add
    local.get 1
    local.get 2
    call 91
    drop
    local.get 0
    local.get 2
    local.get 3
    i32.add
    i32.store offset=8
    i32.const 0)
  (func (;37;) (type 2) (param i32 i32 i32) (result i32)
    (local i32)
    local.get 2
    local.get 0
    i32.load
    local.tee 0
    i32.load
    local.get 0
    i32.load offset=8
    local.tee 3
    i32.sub
    i32.gt_u
    if  ;; label = @1
      local.get 0
      local.get 3
      local.get 2
      call 21
      local.get 0
      i32.load offset=8
      local.set 3
    end
    local.get 0
    i32.load offset=4
    local.get 3
    i32.add
    local.get 1
    local.get 2
    call 91
    drop
    local.get 0
    local.get 2
    local.get 3
    i32.add
    i32.store offset=8
    i32.const 0)
  (func (;38;) (type 4) (param i32)
    (local i32 i32)
    block  ;; label = @1
      local.get 0
      if  ;; label = @2
        local.get 0
        i32.load
        br_if 1 (;@1;)
        local.get 0
        i32.load offset=16
        local.set 1
        local.get 0
        i32.load offset=12
        local.get 0
        call 6
        if  ;; label = @3
          local.get 1
          call 6
        end
        return
      end
      call 83
      unreachable
    end
    call 84
    unreachable)
  (func (;39;) (type 2) (param i32 i32 i32) (result i32)
    (local i32)
    local.get 2
    local.get 0
    i32.load
    local.get 0
    i32.load offset=8
    local.tee 3
    i32.sub
    i32.gt_u
    if  ;; label = @1
      local.get 0
      local.get 3
      local.get 2
      call 19
      local.get 0
      i32.load offset=8
      local.set 3
    end
    local.get 0
    i32.load offset=4
    local.get 3
    i32.add
    local.get 1
    local.get 2
    call 91
    drop
    local.get 0
    local.get 2
    local.get 3
    i32.add
    i32.store offset=8
    i32.const 0)
  (func (;40;) (type 10)
    (local i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 0
    global.set 0
    local.get 0
    i32.const 20
    i32.add
    i32.const 1
    i32.store
    local.get 0
    i32.const 28
    i32.add
    i32.const 0
    i32.store
    local.get 0
    i32.const 1049956
    i32.store offset=16
    local.get 0
    i32.const 1049908
    i32.store offset=24
    local.get 0
    i32.const 0
    i32.store offset=8
    local.get 0
    i32.const 8
    i32.add
    i32.const 1049964
    call 43
    unreachable)
  (func (;41;) (type 1) (param i32 i32)
    (local i32 i32)
    local.get 1
    i32.load offset=4
    local.set 2
    local.get 1
    i32.load
    local.set 3
    i32.const 8
    i32.const 4
    call 73
    local.tee 1
    i32.eqz
    if  ;; label = @1
      i32.const 8
      i32.const 4
      call 90
      unreachable
    end
    local.get 1
    local.get 2
    i32.store offset=4
    local.get 1
    local.get 3
    i32.store
    local.get 0
    i32.const 1049836
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store)
  (func (;42;) (type 11) (param i32 i32 i32 i32) (result i32)
    block  ;; label = @1
      block (result i32)  ;; label = @2
        local.get 2
        i32.const 1114112
        i32.ne
        if  ;; label = @3
          i32.const 1
          local.get 0
          local.get 2
          local.get 1
          i32.load offset=16
          call_indirect (type 0)
          br_if 1 (;@2;)
          drop
        end
        local.get 3
        br_if 1 (;@1;)
        i32.const 0
      end
      return
    end
    local.get 0
    local.get 3
    i32.const 0
    local.get 1
    i32.load offset=12
    call_indirect (type 2))
  (func (;43;) (type 1) (param i32 i32)
    (local i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    i32.const 1
    i32.store8 offset=24
    local.get 2
    local.get 1
    i32.store offset=20
    local.get 2
    local.get 0
    i32.store offset=16
    local.get 2
    i32.const 1050108
    i32.store offset=12
    local.get 2
    i32.const 1049980
    i32.store offset=8
    global.get 0
    i32.const 16
    i32.sub
    local.tee 0
    global.set 0
    block  ;; label = @1
      local.get 2
      i32.const 8
      i32.add
      local.tee 1
      i32.load offset=12
      local.tee 2
      if  ;; label = @2
        local.get 1
        i32.load offset=8
        local.tee 3
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        local.get 2
        i32.store offset=8
        local.get 0
        local.get 1
        i32.store offset=4
        local.get 0
        local.get 3
        i32.store
        global.get 0
        i32.const 16
        i32.sub
        local.tee 1
        global.set 0
        local.get 0
        i32.load
        local.tee 2
        i32.const 20
        i32.add
        i32.load
        local.set 3
        block  ;; label = @3
          block (result i32)  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 2
                i32.const 12
                i32.add
                i32.load
                br_table 0 (;@6;) 1 (;@5;) 3 (;@3;)
              end
              local.get 3
              br_if 2 (;@3;)
              i32.const 0
              local.set 2
              i32.const 1049548
              br 1 (;@4;)
            end
            local.get 3
            br_if 1 (;@3;)
            local.get 2
            i32.load offset=8
            local.tee 3
            i32.load offset=4
            local.set 2
            local.get 3
            i32.load
          end
          local.set 3
          local.get 1
          local.get 2
          i32.store offset=4
          local.get 1
          local.get 3
          i32.store
          local.get 1
          i32.const 1049872
          local.get 0
          i32.load offset=4
          local.tee 1
          i32.load offset=8
          local.get 0
          i32.load offset=8
          local.get 1
          i32.load8_u offset=16
          call 22
          unreachable
        end
        local.get 1
        i32.const 0
        i32.store offset=4
        local.get 1
        local.get 2
        i32.store offset=12
        local.get 1
        i32.const 1049852
        local.get 0
        i32.load offset=4
        local.tee 1
        i32.load offset=8
        local.get 0
        i32.load offset=8
        local.get 1
        i32.load8_u offset=16
        call 22
        unreachable
      end
      i32.const 1049548
      i32.const 1049804
      call 35
      unreachable
    end
    i32.const 1049548
    i32.const 1049788
    call 35
    unreachable)
  (func (;44;) (type 3) (param i32) (result i32)
    block  ;; label = @1
      local.get 0
      i32.const 2147483644
      i32.gt_u
      br_if 0 (;@1;)
      local.get 0
      i32.eqz
      if  ;; label = @2
        i32.const 4
        return
      end
      local.get 0
      local.get 0
      i32.const 2147483645
      i32.lt_u
      i32.const 2
      i32.shl
      call 73
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      return
    end
    unreachable)
  (func (;45;) (type 4) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 0
      if  ;; label = @2
        local.get 0
        i32.load
        br_if 1 (;@1;)
        local.get 0
        i32.const -1
        i32.store
        local.get 0
        i32.const 4
        i32.add
        local.tee 7
        i32.const 12
        i32.add
        i32.load
        local.set 3
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 7
                i32.const 16
                i32.add
                i32.load
                local.tee 1
                i32.eqz
                if  ;; label = @7
                  i32.const 1
                  local.set 2
                  br 1 (;@6;)
                end
                local.get 1
                i32.const 0
                i32.lt_s
                br_if 2 (;@4;)
                local.get 1
                i32.const 1
                call 73
                local.tee 2
                i32.eqz
                br_if 1 (;@5;)
              end
              local.get 2
              local.get 3
              local.get 1
              call 91
              local.set 18
              block  ;; label = @6
                local.get 7
                i32.load offset=4
                local.tee 15
                i32.eqz
                br_if 0 (;@6;)
                local.get 7
                i32.load
                local.tee 5
                i32.eqz
                br_if 0 (;@6;)
                local.get 5
                i32.const 1
                i32.sub
                local.set 22
                local.get 15
                i32.const 1
                i32.sub
                local.set 23
                local.get 3
                local.set 16
                local.get 18
                local.set 17
                loop  ;; label = @7
                  local.get 3
                  local.get 13
                  local.tee 2
                  i32.const 1
                  i32.add
                  local.tee 13
                  i32.const 0
                  local.get 2
                  local.get 23
                  i32.ne
                  select
                  local.get 5
                  i32.mul
                  local.tee 9
                  i32.add
                  local.set 24
                  local.get 2
                  local.get 5
                  i32.mul
                  local.set 19
                  local.get 3
                  local.get 2
                  local.get 15
                  local.get 2
                  select
                  i32.const 1
                  i32.sub
                  local.get 5
                  i32.mul
                  local.tee 10
                  i32.add
                  local.set 25
                  i32.const 0
                  local.set 11
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              block  ;; label = @14
                                block  ;; label = @15
                                  block  ;; label = @16
                                    loop  ;; label = @17
                                      local.get 14
                                      local.get 11
                                      local.tee 2
                                      i32.add
                                      local.tee 11
                                      local.get 1
                                      i32.ge_u
                                      br_if 8 (;@9;)
                                      local.get 1
                                      local.get 2
                                      local.get 5
                                      local.get 2
                                      select
                                      i32.const 1
                                      i32.sub
                                      local.tee 4
                                      local.get 10
                                      i32.add
                                      local.tee 8
                                      i32.gt_u
                                      if  ;; label = @18
                                        local.get 2
                                        local.get 10
                                        i32.add
                                        local.get 1
                                        i32.ge_u
                                        br_if 2 (;@16;)
                                        local.get 2
                                        i32.const 1
                                        i32.add
                                        local.tee 11
                                        i32.const 0
                                        local.get 2
                                        local.get 22
                                        i32.ne
                                        select
                                        local.tee 6
                                        local.get 10
                                        i32.add
                                        local.tee 12
                                        local.get 1
                                        i32.ge_u
                                        br_if 3 (;@15;)
                                        local.get 4
                                        local.get 19
                                        i32.add
                                        local.tee 20
                                        local.get 1
                                        i32.ge_u
                                        br_if 4 (;@14;)
                                        local.get 6
                                        local.get 19
                                        i32.add
                                        local.tee 21
                                        local.get 1
                                        i32.ge_u
                                        br_if 5 (;@13;)
                                        local.get 4
                                        local.get 9
                                        i32.add
                                        local.tee 4
                                        local.get 1
                                        i32.ge_u
                                        br_if 6 (;@12;)
                                        local.get 2
                                        local.get 9
                                        i32.add
                                        local.get 1
                                        i32.ge_u
                                        br_if 7 (;@11;)
                                        local.get 6
                                        local.get 9
                                        i32.add
                                        local.tee 6
                                        local.get 1
                                        i32.ge_u
                                        br_if 8 (;@10;)
                                        local.get 3
                                        local.get 6
                                        i32.add
                                        i32.load8_u
                                        local.get 2
                                        local.get 24
                                        i32.add
                                        i32.load8_u
                                        local.get 3
                                        local.get 4
                                        i32.add
                                        i32.load8_u
                                        local.get 3
                                        local.get 21
                                        i32.add
                                        i32.load8_u
                                        local.get 3
                                        local.get 20
                                        i32.add
                                        i32.load8_u
                                        local.get 3
                                        local.get 12
                                        i32.add
                                        i32.load8_u
                                        local.get 2
                                        local.get 25
                                        i32.add
                                        i32.load8_u
                                        local.get 3
                                        local.get 8
                                        i32.add
                                        i32.load8_u
                                        i32.add
                                        i32.add
                                        i32.add
                                        i32.add
                                        i32.add
                                        i32.add
                                        i32.add
                                        local.set 8
                                        block  ;; label = @19
                                          block  ;; label = @20
                                            local.get 2
                                            local.get 16
                                            i32.add
                                            i32.load8_u
                                            local.tee 6
                                            i32.eqz
                                            if  ;; label = @21
                                              i32.const 1
                                              local.set 4
                                              local.get 8
                                              i32.const 255
                                              i32.and
                                              i32.const 3
                                              i32.ne
                                              br_if 1 (;@20;)
                                              br 2 (;@19;)
                                            end
                                            i32.const 0
                                            local.set 4
                                            local.get 8
                                            i32.const 255
                                            i32.and
                                            local.tee 12
                                            i32.const 2
                                            i32.lt_u
                                            br_if 1 (;@19;)
                                            i32.const 1
                                            local.set 4
                                            local.get 8
                                            i32.const 14
                                            i32.and
                                            i32.const 2
                                            i32.eq
                                            br_if 1 (;@19;)
                                            i32.const 0
                                            local.set 4
                                            local.get 12
                                            i32.const 3
                                            i32.gt_u
                                            br_if 1 (;@19;)
                                          end
                                          local.get 6
                                          i32.const 0
                                          i32.ne
                                          local.set 4
                                        end
                                        local.get 2
                                        local.get 17
                                        i32.add
                                        local.get 4
                                        i32.store8
                                        local.get 5
                                        local.get 11
                                        i32.eq
                                        br_if 10 (;@8;)
                                        br 1 (;@17;)
                                      end
                                    end
                                    local.get 8
                                    local.get 1
                                    i32.const 1048588
                                    call 28
                                    unreachable
                                  end
                                  local.get 1
                                  local.get 10
                                  local.get 1
                                  local.get 10
                                  i32.gt_u
                                  select
                                  local.get 1
                                  i32.const 1048604
                                  call 28
                                  unreachable
                                end
                                local.get 12
                                local.get 1
                                i32.const 1048620
                                call 28
                                unreachable
                              end
                              local.get 20
                              local.get 1
                              i32.const 1048636
                              call 28
                              unreachable
                            end
                            local.get 21
                            local.get 1
                            i32.const 1048652
                            call 28
                            unreachable
                          end
                          local.get 4
                          local.get 1
                          i32.const 1048668
                          call 28
                          unreachable
                        end
                        local.get 1
                        local.get 9
                        local.get 1
                        local.get 9
                        i32.gt_u
                        select
                        local.get 1
                        i32.const 1048684
                        call 28
                        unreachable
                      end
                      local.get 6
                      local.get 1
                      i32.const 1048700
                      call 28
                      unreachable
                    end
                    local.get 11
                    local.get 1
                    i32.const 1048716
                    call 28
                    unreachable
                  end
                  local.get 5
                  local.get 16
                  i32.add
                  local.set 16
                  local.get 5
                  local.get 17
                  i32.add
                  local.set 17
                  local.get 5
                  local.get 14
                  i32.add
                  local.set 14
                  local.get 13
                  local.get 15
                  i32.ne
                  br_if 0 (;@7;)
                end
              end
              local.get 7
              i32.load offset=8
              if  ;; label = @6
                local.get 3
                call 6
              end
              local.get 7
              local.get 1
              i32.store offset=8
              local.get 7
              i32.const 12
              i32.add
              local.get 18
              i32.store
              br 2 (;@3;)
            end
            local.get 1
            i32.const 1
            call 90
            unreachable
          end
          call 40
          unreachable
        end
        local.get 0
        i32.const 0
        i32.store
        return
      end
      call 83
      unreachable
    end
    call 84
    unreachable)
  (func (;46;) (type 3) (param i32) (result i32)
    block  ;; label = @1
      local.get 0
      if  ;; label = @2
        local.get 0
        i32.load
        i32.const -1
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.const 8
        i32.add
        i32.load
        return
      end
      call 83
      unreachable
    end
    call 84
    unreachable)
  (func (;47;) (type 3) (param i32) (result i32)
    block  ;; label = @1
      local.get 0
      if  ;; label = @2
        local.get 0
        i32.load
        i32.const -1
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.const 16
        i32.add
        i32.load
        return
      end
      call 83
      unreachable
    end
    call 84
    unreachable)
  (func (;48;) (type 1) (param i32 i32)
    local.get 0
    local.get 0
    i32.load offset=4
    i32.const 1
    i32.and
    local.get 1
    i32.or
    i32.const 2
    i32.or
    i32.store offset=4
    local.get 0
    local.get 1
    i32.add
    local.tee 0
    local.get 0
    i32.load offset=4
    i32.const 1
    i32.or
    i32.store offset=4)
  (func (;49;) (type 3) (param i32) (result i32)
    block  ;; label = @1
      local.get 0
      if  ;; label = @2
        local.get 0
        i32.load
        i32.const -1
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.load offset=4
        return
      end
      call 83
      unreachable
    end
    call 84
    unreachable)
  (func (;50;) (type 4) (param i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.const 4
      i32.add
      i32.load
      local.tee 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.load
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      call 6
    end)
  (func (;51;) (type 2) (param i32 i32 i32) (result i32)
    block  ;; label = @1
      local.get 1
      i32.const 2147483644
      i32.le_u
      if  ;; label = @2
        local.get 0
        local.get 1
        i32.const 4
        local.get 2
        call 67
        local.tee 0
        br_if 1 (;@1;)
      end
      unreachable
    end
    local.get 0)
  (func (;52;) (type 5) (param i32 i32 i32)
    local.get 2
    local.get 2
    i32.load offset=4
    i32.const -2
    i32.and
    i32.store offset=4
    local.get 0
    local.get 1
    i32.const 1
    i32.or
    i32.store offset=4
    local.get 0
    local.get 1
    i32.add
    local.get 1
    i32.store)
  (func (;53;) (type 14) (param i32 i32 i32 i32 i32 i32) (result i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 1049366
      i32.const 50
      call 85
      unreachable
    end
    local.get 0
    local.get 2
    local.get 3
    local.get 4
    local.get 5
    local.get 1
    i32.load offset=16
    call_indirect (type 12))
  (func (;54;) (type 8) (param i32 i32 i32 i32 i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 1049366
      i32.const 50
      call 85
      unreachable
    end
    local.get 0
    local.get 2
    local.get 3
    local.get 4
    local.get 1
    i32.load offset=16
    call_indirect (type 9))
  (func (;55;) (type 12) (param i32 i32 i32 i32 i32) (result i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 1049366
      i32.const 50
      call 85
      unreachable
    end
    local.get 0
    local.get 2
    local.get 3
    local.get 4
    local.get 1
    i32.load offset=16
    call_indirect (type 11))
  (func (;56;) (type 15) (param i32 i32 f64 i32 i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 1049366
      i32.const 50
      call 85
      unreachable
    end
    local.get 0
    local.get 2
    local.get 3
    local.get 4
    local.get 1
    i32.load offset=16
    call_indirect (type 16))
  (func (;57;) (type 17) (param i32 i32 f32 i32 i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 1049366
      i32.const 50
      call 85
      unreachable
    end
    local.get 0
    local.get 2
    local.get 3
    local.get 4
    local.get 1
    i32.load offset=16
    call_indirect (type 18))
  (func (;58;) (type 19) (param i32 i32 i64 i32 i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 1049366
      i32.const 50
      call 85
      unreachable
    end
    local.get 0
    local.get 2
    local.get 3
    local.get 4
    local.get 1
    i32.load offset=16
    call_indirect (type 20))
  (func (;59;) (type 1) (param i32 i32)
    local.get 0
    local.get 1
    i32.const 3
    i32.or
    i32.store offset=4
    local.get 0
    local.get 1
    i32.add
    local.tee 0
    local.get 0
    i32.load offset=4
    i32.const 1
    i32.or
    i32.store offset=4)
  (func (;60;) (type 4) (param i32)
    local.get 0
    i32.load
    if  ;; label = @1
      local.get 0
      i32.const 4
      i32.add
      i32.load
      call 6
    end)
  (func (;61;) (type 9) (param i32 i32 i32 i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 1049366
      i32.const 50
      call 85
      unreachable
    end
    local.get 0
    local.get 2
    local.get 3
    local.get 1
    i32.load offset=16
    call_indirect (type 5))
  (func (;62;) (type 2) (param i32 i32 i32) (result i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      i32.const 1049366
      i32.const 50
      call 85
      unreachable
    end
    local.get 0
    local.get 2
    local.get 1
    i32.load offset=16
    call_indirect (type 0))
  (func (;63;) (type 3) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.load offset=16
    local.tee 1
    if (result i32)  ;; label = @1
      local.get 1
    else
      local.get 0
      i32.const 20
      i32.add
      i32.load
    end)
  (func (;64;) (type 3) (param i32) (result i32)
    i32.const 25
    local.get 0
    i32.const 1
    i32.shr_u
    i32.sub
    i32.const 0
    local.get 0
    i32.const 31
    i32.ne
    select)
  (func (;65;) (type 1) (param i32 i32)
    local.get 0
    local.get 1
    i32.const 1
    i32.or
    i32.store offset=4
    local.get 0
    local.get 1
    i32.add
    local.get 1
    i32.store)
  (func (;66;) (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add
    i32.const 1
    i32.sub
    i32.const 0
    local.get 1
    i32.sub
    i32.and)
  (func (;67;) (type 11) (param i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    block (result i32)  ;; label = @1
      local.get 0
      local.set 5
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 2
            i32.const 9
            i32.ge_u
            if  ;; label = @5
              local.get 3
              local.get 2
              call 10
              local.tee 7
              br_if 1 (;@4;)
              i32.const 0
              br 4 (;@1;)
            end
            i32.const 8
            i32.const 8
            call 66
            local.set 0
            i32.const 20
            i32.const 8
            call 66
            local.set 1
            i32.const 16
            i32.const 8
            call 66
            local.set 2
            i32.const 0
            i32.const 16
            i32.const 8
            call 66
            i32.const 2
            i32.shl
            i32.sub
            local.tee 4
            i32.const -65536
            local.get 2
            local.get 0
            local.get 1
            i32.add
            i32.add
            i32.sub
            i32.const -9
            i32.and
            i32.const 3
            i32.sub
            local.tee 0
            local.get 0
            local.get 4
            i32.gt_u
            select
            local.get 3
            i32.le_u
            br_if 1 (;@3;)
            i32.const 16
            local.get 3
            i32.const 4
            i32.add
            i32.const 16
            i32.const 8
            call 66
            i32.const 5
            i32.sub
            local.get 3
            i32.gt_u
            select
            i32.const 8
            call 66
            local.set 2
            local.get 5
            call 97
            local.tee 0
            local.get 0
            call 86
            local.tee 4
            call 94
            local.set 1
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 0
                          call 78
                          i32.eqz
                          if  ;; label = @12
                            local.get 2
                            local.get 4
                            i32.le_u
                            br_if 1 (;@11;)
                            local.get 1
                            i32.const 1050840
                            i32.load
                            i32.eq
                            br_if 2 (;@10;)
                            local.get 1
                            i32.const 1050836
                            i32.load
                            i32.eq
                            br_if 3 (;@9;)
                            local.get 1
                            call 75
                            br_if 7 (;@5;)
                            local.get 1
                            call 86
                            local.tee 6
                            local.get 4
                            i32.add
                            local.tee 8
                            local.get 2
                            i32.lt_u
                            br_if 7 (;@5;)
                            local.get 8
                            local.get 2
                            i32.sub
                            local.set 4
                            local.get 6
                            i32.const 256
                            i32.lt_u
                            br_if 4 (;@8;)
                            local.get 1
                            call 14
                            br 5 (;@7;)
                          end
                          local.get 0
                          call 86
                          local.set 1
                          local.get 2
                          i32.const 256
                          i32.lt_u
                          br_if 6 (;@5;)
                          local.get 1
                          local.get 2
                          i32.sub
                          i32.const 131073
                          i32.lt_u
                          local.get 2
                          i32.const 4
                          i32.add
                          local.get 1
                          i32.le_u
                          i32.and
                          br_if 5 (;@6;)
                          local.get 1
                          local.get 0
                          i32.load
                          local.tee 1
                          i32.add
                          i32.const 16
                          i32.add
                          local.set 4
                          local.get 2
                          i32.const 31
                          i32.add
                          i32.const 65536
                          call 66
                          local.set 2
                          br 6 (;@5;)
                        end
                        i32.const 16
                        i32.const 8
                        call 66
                        local.get 4
                        local.get 2
                        i32.sub
                        local.tee 1
                        i32.gt_u
                        br_if 4 (;@6;)
                        local.get 0
                        local.get 2
                        call 94
                        local.set 4
                        local.get 0
                        local.get 2
                        call 48
                        local.get 4
                        local.get 1
                        call 48
                        local.get 4
                        local.get 1
                        call 9
                        br 4 (;@6;)
                      end
                      i32.const 1050832
                      i32.load
                      local.get 4
                      i32.add
                      local.tee 4
                      local.get 2
                      i32.le_u
                      br_if 4 (;@5;)
                      local.get 0
                      local.get 2
                      call 94
                      local.set 1
                      local.get 0
                      local.get 2
                      call 48
                      local.get 1
                      local.get 4
                      local.get 2
                      i32.sub
                      local.tee 2
                      i32.const 1
                      i32.or
                      i32.store offset=4
                      i32.const 1050832
                      local.get 2
                      i32.store
                      i32.const 1050840
                      local.get 1
                      i32.store
                      br 3 (;@6;)
                    end
                    i32.const 1050828
                    i32.load
                    local.get 4
                    i32.add
                    local.tee 4
                    local.get 2
                    i32.lt_u
                    br_if 3 (;@5;)
                    block  ;; label = @9
                      i32.const 16
                      i32.const 8
                      call 66
                      local.get 4
                      local.get 2
                      i32.sub
                      local.tee 1
                      i32.gt_u
                      if  ;; label = @10
                        local.get 0
                        local.get 4
                        call 48
                        i32.const 0
                        local.set 1
                        i32.const 0
                        local.set 4
                        br 1 (;@9;)
                      end
                      local.get 0
                      local.get 2
                      call 94
                      local.tee 4
                      local.get 1
                      call 94
                      local.set 6
                      local.get 0
                      local.get 2
                      call 48
                      local.get 4
                      local.get 1
                      call 65
                      local.get 6
                      local.get 6
                      i32.load offset=4
                      i32.const -2
                      i32.and
                      i32.store offset=4
                    end
                    i32.const 1050836
                    local.get 4
                    i32.store
                    i32.const 1050828
                    local.get 1
                    i32.store
                    br 2 (;@6;)
                  end
                  local.get 1
                  i32.const 12
                  i32.add
                  i32.load
                  local.tee 9
                  local.get 1
                  i32.const 8
                  i32.add
                  i32.load
                  local.tee 1
                  i32.ne
                  if  ;; label = @8
                    local.get 1
                    local.get 9
                    i32.store offset=12
                    local.get 9
                    local.get 1
                    i32.store offset=8
                    br 1 (;@7;)
                  end
                  i32.const 1050820
                  i32.const 1050820
                  i32.load
                  i32.const -2
                  local.get 6
                  i32.const 3
                  i32.shr_u
                  i32.rotl
                  i32.and
                  i32.store
                end
                i32.const 16
                i32.const 8
                call 66
                local.get 4
                i32.le_u
                if  ;; label = @7
                  local.get 0
                  local.get 2
                  call 94
                  local.set 1
                  local.get 0
                  local.get 2
                  call 48
                  local.get 1
                  local.get 4
                  call 48
                  local.get 1
                  local.get 4
                  call 9
                  br 1 (;@6;)
                end
                local.get 0
                local.get 8
                call 48
              end
              local.get 0
              br_if 3 (;@2;)
            end
            local.get 3
            call 5
            local.tee 1
            i32.eqz
            br_if 1 (;@3;)
            local.get 1
            local.get 5
            local.get 0
            call 86
            i32.const -8
            i32.const -4
            local.get 0
            call 78
            select
            i32.add
            local.tee 0
            local.get 3
            local.get 0
            local.get 3
            i32.lt_u
            select
            call 91
            local.get 5
            call 6
            br 3 (;@1;)
          end
          local.get 7
          local.get 5
          local.get 1
          local.get 3
          local.get 1
          local.get 3
          i32.lt_u
          select
          call 91
          drop
          local.get 5
          call 6
        end
        local.get 7
        br 1 (;@1;)
      end
      local.get 0
      call 78
      drop
      local.get 0
      call 96
    end)
  (func (;68;) (type 1) (param i32 i32)
    local.get 1
    if  ;; label = @1
      local.get 0
      call 6
    end)
  (func (;69;) (type 3) (param i32) (result i32)
    local.get 0
    i32.const 1
    i32.shl
    local.tee 0
    i32.const 0
    local.get 0
    i32.sub
    i32.or)
  (func (;70;) (type 0) (param i32 i32) (result i32)
    local.get 1
    local.get 0
    i32.load
    local.tee 0
    i32.load
    local.get 0
    i32.load offset=4
    call 7)
  (func (;71;) (type 0) (param i32 i32) (result i32)
    local.get 1
    i32.load
    i32.const 1050344
    i32.const 5
    local.get 1
    i32.load offset=4
    i32.load offset=12
    call_indirect (type 2))
  (func (;72;) (type 0) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.get 1
    local.get 0
    i32.load offset=4
    i32.load offset=12
    call_indirect (type 0))
  (func (;73;) (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call 10)
  (func (;74;) (type 1) (param i32 i32)
    local.get 0
    i32.const 1049836
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store)
  (func (;75;) (type 3) (param i32) (result i32)
    local.get 0
    i32.load8_u offset=4
    i32.const 2
    i32.and
    i32.const 1
    i32.shr_u)
  (func (;76;) (type 0) (param i32 i32) (result i32)
    local.get 1
    local.get 0
    i32.load
    local.get 0
    i32.load offset=4
    call 7)
  (func (;77;) (type 3) (param i32) (result i32)
    i32.const 0
    local.get 0
    i32.sub
    local.get 0
    i32.and)
  (func (;78;) (type 3) (param i32) (result i32)
    local.get 0
    i32.load8_u offset=4
    i32.const 3
    i32.and
    i32.eqz)
  (func (;79;) (type 1) (param i32 i32)
    local.get 0
    local.get 1
    i32.const 3
    i32.or
    i32.store offset=4)
  (func (;80;) (type 3) (param i32) (result i32)
    local.get 0
    i32.load
    local.get 0
    i32.load offset=4
    i32.add)
  (func (;81;) (type 0) (param i32 i32) (result i32)
    local.get 0
    i32.load
    drop
    loop  ;; label = @1
      br 0 (;@1;)
    end
    unreachable)
  (func (;82;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64 i64)
    local.get 0
    i64.load32_u
    local.set 13
    global.get 0
    i32.const 48
    i32.sub
    local.tee 4
    global.set 0
    i32.const 39
    local.set 0
    block  ;; label = @1
      local.get 13
      i64.const 10000
      i64.lt_u
      if  ;; label = @2
        local.get 13
        local.set 14
        br 1 (;@1;)
      end
      loop  ;; label = @2
        local.get 4
        i32.const 9
        i32.add
        local.get 0
        i32.add
        local.tee 2
        i32.const 4
        i32.sub
        local.get 13
        local.get 13
        i64.const 10000
        i64.div_u
        local.tee 14
        i64.const 10000
        i64.mul
        i64.sub
        i32.wrap_i64
        local.tee 3
        i32.const 65535
        i32.and
        i32.const 100
        i32.div_u
        local.tee 5
        i32.const 1
        i32.shl
        i32.const 1050144
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        local.get 2
        i32.const 2
        i32.sub
        local.get 3
        local.get 5
        i32.const 100
        i32.mul
        i32.sub
        i32.const 65535
        i32.and
        i32.const 1
        i32.shl
        i32.const 1050144
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        local.get 0
        i32.const 4
        i32.sub
        local.set 0
        local.get 13
        i64.const 99999999
        i64.gt_u
        local.get 14
        local.set 13
        br_if 0 (;@2;)
      end
    end
    local.get 14
    i32.wrap_i64
    local.tee 2
    i32.const 99
    i32.gt_u
    if  ;; label = @1
      local.get 0
      i32.const 2
      i32.sub
      local.tee 0
      local.get 4
      i32.const 9
      i32.add
      i32.add
      local.get 14
      i32.wrap_i64
      local.tee 2
      local.get 2
      i32.const 65535
      i32.and
      i32.const 100
      i32.div_u
      local.tee 2
      i32.const 100
      i32.mul
      i32.sub
      i32.const 65535
      i32.and
      i32.const 1
      i32.shl
      i32.const 1050144
      i32.add
      i32.load16_u align=1
      i32.store16 align=1
    end
    block  ;; label = @1
      local.get 2
      i32.const 10
      i32.ge_u
      if  ;; label = @2
        local.get 0
        i32.const 2
        i32.sub
        local.tee 0
        local.get 4
        i32.const 9
        i32.add
        i32.add
        local.get 2
        i32.const 1
        i32.shl
        i32.const 1050144
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        br 1 (;@1;)
      end
      local.get 0
      i32.const 1
      i32.sub
      local.tee 0
      local.get 4
      i32.const 9
      i32.add
      i32.add
      local.get 2
      i32.const 48
      i32.add
      i32.store8
    end
    block (result i32)  ;; label = @1
      local.get 4
      i32.const 9
      i32.add
      local.get 0
      i32.add
      local.set 8
      i32.const 43
      i32.const 1114112
      local.get 1
      i32.load offset=24
      local.tee 3
      i32.const 1
      i32.and
      local.tee 2
      select
      local.set 5
      local.get 2
      i32.const 39
      local.get 0
      i32.sub
      local.tee 9
      i32.add
      local.set 2
      i32.const 1049980
      i32.const 0
      local.get 3
      i32.const 4
      i32.and
      select
      local.set 7
      block  ;; label = @2
        block  ;; label = @3
          local.get 1
          i32.load offset=8
          i32.eqz
          if  ;; label = @4
            i32.const 1
            local.set 0
            local.get 1
            i32.load
            local.tee 3
            local.get 1
            i32.const 4
            i32.add
            i32.load
            local.tee 2
            local.get 5
            local.get 7
            call 42
            br_if 1 (;@3;)
            br 2 (;@2;)
          end
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 2
                  local.get 1
                  i32.const 12
                  i32.add
                  i32.load
                  local.tee 6
                  i32.lt_u
                  if  ;; label = @8
                    local.get 3
                    i32.const 8
                    i32.and
                    br_if 4 (;@4;)
                    local.get 6
                    local.get 2
                    i32.sub
                    local.tee 2
                    local.set 3
                    i32.const 1
                    local.get 1
                    i32.load8_u offset=32
                    local.tee 0
                    local.get 0
                    i32.const 3
                    i32.eq
                    select
                    i32.const 3
                    i32.and
                    local.tee 0
                    i32.const 1
                    i32.sub
                    br_table 1 (;@7;) 2 (;@6;) 3 (;@5;)
                  end
                  i32.const 1
                  local.set 0
                  local.get 1
                  i32.load
                  local.tee 3
                  local.get 1
                  i32.const 4
                  i32.add
                  i32.load
                  local.tee 2
                  local.get 5
                  local.get 7
                  call 42
                  br_if 4 (;@3;)
                  br 5 (;@2;)
                end
                i32.const 0
                local.set 3
                local.get 2
                local.set 0
                br 1 (;@5;)
              end
              local.get 2
              i32.const 1
              i32.shr_u
              local.set 0
              local.get 2
              i32.const 1
              i32.add
              i32.const 1
              i32.shr_u
              local.set 3
            end
            local.get 0
            i32.const 1
            i32.add
            local.set 0
            local.get 1
            i32.const 4
            i32.add
            i32.load
            local.set 2
            local.get 1
            i32.load offset=28
            local.set 6
            local.get 1
            i32.load
            local.set 1
            block  ;; label = @5
              loop  ;; label = @6
                local.get 0
                i32.const 1
                i32.sub
                local.tee 0
                i32.eqz
                br_if 1 (;@5;)
                local.get 1
                local.get 6
                local.get 2
                i32.load offset=16
                call_indirect (type 0)
                i32.eqz
                br_if 0 (;@6;)
              end
              i32.const 1
              br 4 (;@1;)
            end
            i32.const 1
            local.set 0
            local.get 6
            i32.const 1114112
            i32.eq
            br_if 1 (;@3;)
            local.get 1
            local.get 2
            local.get 5
            local.get 7
            call 42
            br_if 1 (;@3;)
            local.get 1
            local.get 8
            local.get 9
            local.get 2
            i32.load offset=12
            call_indirect (type 2)
            br_if 1 (;@3;)
            i32.const 0
            local.set 0
            block (result i32)  ;; label = @5
              loop  ;; label = @6
                local.get 3
                local.get 0
                local.get 3
                i32.eq
                br_if 1 (;@5;)
                drop
                local.get 0
                i32.const 1
                i32.add
                local.set 0
                local.get 1
                local.get 6
                local.get 2
                i32.load offset=16
                call_indirect (type 0)
                i32.eqz
                br_if 0 (;@6;)
              end
              local.get 0
              i32.const 1
              i32.sub
            end
            local.get 3
            i32.lt_u
            local.set 0
            br 1 (;@3;)
          end
          local.get 1
          i32.load offset=28
          local.set 11
          local.get 1
          i32.const 48
          i32.store offset=28
          local.get 1
          i32.load8_u offset=32
          local.set 12
          i32.const 1
          local.set 0
          local.get 1
          i32.const 1
          i32.store8 offset=32
          local.get 1
          i32.load
          local.tee 3
          local.get 1
          i32.const 4
          i32.add
          i32.load
          local.tee 10
          local.get 5
          local.get 7
          call 42
          br_if 0 (;@3;)
          local.get 6
          local.get 2
          i32.sub
          i32.const 1
          i32.add
          local.set 0
          block  ;; label = @4
            loop  ;; label = @5
              local.get 0
              i32.const 1
              i32.sub
              local.tee 0
              i32.eqz
              br_if 1 (;@4;)
              local.get 3
              i32.const 48
              local.get 10
              i32.load offset=16
              call_indirect (type 0)
              i32.eqz
              br_if 0 (;@5;)
            end
            i32.const 1
            br 3 (;@1;)
          end
          i32.const 1
          local.set 0
          local.get 3
          local.get 8
          local.get 9
          local.get 10
          i32.load offset=12
          call_indirect (type 2)
          br_if 0 (;@3;)
          local.get 1
          local.get 12
          i32.store8 offset=32
          local.get 1
          local.get 11
          i32.store offset=28
          i32.const 0
          br 2 (;@1;)
        end
        local.get 0
        br 1 (;@1;)
      end
      local.get 3
      local.get 8
      local.get 9
      local.get 2
      i32.load offset=12
      call_indirect (type 2)
    end
    local.get 4
    i32.const 48
    i32.add
    global.set 0)
  (func (;83;) (type 10)
    i32.const 1049416
    i32.const 27
    call 85
    unreachable)
  (func (;84;) (type 10)
    i32.const 1049443
    i32.const 79
    call 85
    unreachable)
  (func (;85;) (type 1) (param i32 i32)
    local.get 0
    local.get 1
    call 4
    unreachable)
  (func (;86;) (type 3) (param i32) (result i32)
    local.get 0
    i32.load offset=4
    i32.const -8
    i32.and)
  (func (;87;) (type 3) (param i32) (result i32)
    local.get 0
    i32.load offset=4
    i32.const 1
    i32.and)
  (func (;88;) (type 3) (param i32) (result i32)
    local.get 0
    i32.load offset=12
    i32.const 1
    i32.and)
  (func (;89;) (type 3) (param i32) (result i32)
    local.get 0
    i32.load offset=12
    i32.const 1
    i32.shr_u)
  (func (;90;) (type 1) (param i32 i32)
    local.get 0
    local.get 1
    i32.const 1050388
    i32.load
    local.tee 0
    i32.const 28
    local.get 0
    select
    call_indirect (type 1)
    unreachable)
  (func (;91;) (type 2) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 2
      local.tee 4
      i32.const 15
      i32.le_u
      if  ;; label = @2
        local.get 0
        local.set 2
        br 1 (;@1;)
      end
      local.get 0
      i32.const 0
      local.get 0
      i32.sub
      i32.const 3
      i32.and
      local.tee 3
      i32.add
      local.set 5
      local.get 3
      if  ;; label = @2
        local.get 0
        local.set 2
        local.get 1
        local.set 6
        loop  ;; label = @3
          local.get 2
          local.get 6
          i32.load8_u
          i32.store8
          local.get 6
          i32.const 1
          i32.add
          local.set 6
          local.get 2
          i32.const 1
          i32.add
          local.tee 2
          local.get 5
          i32.lt_u
          br_if 0 (;@3;)
        end
      end
      local.get 5
      local.get 4
      local.get 3
      i32.sub
      local.tee 8
      i32.const -4
      i32.and
      local.tee 7
      i32.add
      local.set 2
      block  ;; label = @2
        local.get 1
        local.get 3
        i32.add
        local.tee 3
        i32.const 3
        i32.and
        local.tee 4
        if  ;; label = @3
          local.get 7
          i32.const 0
          i32.le_s
          br_if 1 (;@2;)
          local.get 3
          i32.const -4
          i32.and
          local.tee 6
          i32.const 4
          i32.add
          local.set 1
          i32.const 0
          local.get 4
          i32.const 3
          i32.shl
          local.tee 9
          i32.sub
          i32.const 24
          i32.and
          local.set 4
          local.get 6
          i32.load
          local.set 6
          loop  ;; label = @4
            local.get 5
            local.get 6
            local.get 9
            i32.shr_u
            local.get 1
            i32.load
            local.tee 6
            local.get 4
            i32.shl
            i32.or
            i32.store
            local.get 1
            i32.const 4
            i32.add
            local.set 1
            local.get 5
            i32.const 4
            i32.add
            local.tee 5
            local.get 2
            i32.lt_u
            br_if 0 (;@4;)
          end
          br 1 (;@2;)
        end
        local.get 7
        i32.const 0
        i32.le_s
        br_if 0 (;@2;)
        local.get 3
        local.set 1
        loop  ;; label = @3
          local.get 5
          local.get 1
          i32.load
          i32.store
          local.get 1
          i32.const 4
          i32.add
          local.set 1
          local.get 5
          i32.const 4
          i32.add
          local.tee 5
          local.get 2
          i32.lt_u
          br_if 0 (;@3;)
        end
      end
      local.get 8
      i32.const 3
      i32.and
      local.set 4
      local.get 3
      local.get 7
      i32.add
      local.set 1
    end
    local.get 4
    if  ;; label = @1
      local.get 2
      local.get 4
      i32.add
      local.set 3
      loop  ;; label = @2
        local.get 2
        local.get 1
        i32.load8_u
        i32.store8
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 2
        i32.const 1
        i32.add
        local.tee 2
        local.get 3
        i32.lt_u
        br_if 0 (;@2;)
      end
    end
    local.get 0)
  (func (;92;) (type 1) (param i32 i32)
    (local i32 i32)
    local.get 1
    i32.const 15
    i32.gt_u
    if  ;; label = @1
      local.get 0
      i32.const 0
      local.get 0
      i32.sub
      i32.const 3
      i32.and
      local.tee 3
      i32.add
      local.set 2
      local.get 3
      if  ;; label = @2
        loop  ;; label = @3
          local.get 0
          i32.const 0
          i32.store8
          local.get 0
          i32.const 1
          i32.add
          local.tee 0
          local.get 2
          i32.lt_u
          br_if 0 (;@3;)
        end
      end
      local.get 2
      local.get 1
      local.get 3
      i32.sub
      local.tee 1
      i32.const -4
      i32.and
      local.tee 3
      i32.add
      local.set 0
      local.get 3
      i32.const 0
      i32.gt_s
      if  ;; label = @2
        loop  ;; label = @3
          local.get 2
          i32.const 0
          i32.store
          local.get 2
          i32.const 4
          i32.add
          local.tee 2
          local.get 0
          i32.lt_u
          br_if 0 (;@3;)
        end
      end
      local.get 1
      i32.const 3
      i32.and
      local.set 1
    end
    local.get 1
    if  ;; label = @1
      local.get 0
      local.get 1
      i32.add
      local.set 1
      loop  ;; label = @2
        local.get 0
        i32.const 0
        i32.store8
        local.get 0
        i32.const 1
        i32.add
        local.tee 0
        local.get 1
        i32.lt_u
        br_if 0 (;@2;)
      end
    end)
  (func (;93;) (type 6) (result i32)
    i32.const 1050864
    i32.load
    i32.eqz)
  (func (;94;) (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add)
  (func (;95;) (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.sub)
  (func (;96;) (type 3) (param i32) (result i32)
    local.get 0
    i32.const 8
    i32.add)
  (func (;97;) (type 3) (param i32) (result i32)
    local.get 0
    i32.const 8
    i32.sub)
  (func (;98;) (type 1) (param i32 i32)
    (local i32 i32 i32 i32 i32)
    block  ;; label = @1
      global.get 0
      i32.const 80
      i32.sub
      local.tee 2
      global.set 0
      local.get 2
      i32.const 0
      i32.store offset=24
      local.get 2
      i64.const 4294967296
      i64.store offset=16
      local.get 2
      i32.const 32
      i32.add
      local.tee 4
      local.tee 0
      i32.const 3
      i32.store8 offset=32
      local.get 0
      i64.const 137438953472
      i64.store offset=24 align=4
      local.get 0
      i32.const 0
      i32.store offset=16
      local.get 0
      i32.const 0
      i32.store offset=8
      local.get 0
      i32.const 1049168
      i32.store offset=4
      local.get 0
      local.get 2
      i32.const 16
      i32.add
      i32.store
      global.get 0
      i32.const -64
      i32.add
      local.tee 0
      global.set 0
      i32.const 1
      local.set 3
      block  ;; label = @2
        local.get 4
        i32.load
        local.tee 5
        i32.const 1050076
        i32.const 12
        local.get 4
        i32.load offset=4
        local.tee 4
        i32.load offset=12
        call_indirect (type 2)
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 1
          i32.load offset=8
          local.tee 3
          if  ;; label = @4
            local.get 0
            local.get 3
            i32.store offset=12
            local.get 0
            i32.const 45
            i32.store offset=20
            local.get 0
            local.get 0
            i32.const 12
            i32.add
            i32.store offset=16
            i32.const 1
            local.set 3
            local.get 0
            i32.const 1
            i32.store offset=60
            local.get 0
            i32.const 2
            i32.store offset=52
            local.get 0
            i32.const 1050092
            i32.store offset=48
            local.get 0
            i32.const 0
            i32.store offset=40
            local.get 0
            local.get 0
            i32.const 16
            i32.add
            i32.store offset=56
            local.get 5
            local.get 4
            local.get 0
            i32.const 40
            i32.add
            call 8
            i32.eqz
            br_if 1 (;@3;)
            br 2 (;@2;)
          end
          local.get 1
          i32.load
          local.tee 3
          local.get 1
          i32.load offset=4
          i32.const 12
          i32.add
          i32.load
          call_indirect (type 7)
          i64.const -8527728395957036344
          i64.ne
          br_if 0 (;@3;)
          local.get 0
          local.get 3
          i32.store offset=12
          local.get 0
          i32.const 46
          i32.store offset=20
          local.get 0
          local.get 0
          i32.const 12
          i32.add
          i32.store offset=16
          i32.const 1
          local.set 3
          local.get 0
          i32.const 1
          i32.store offset=60
          local.get 0
          i32.const 2
          i32.store offset=52
          local.get 0
          i32.const 1050092
          i32.store offset=48
          local.get 0
          i32.const 0
          i32.store offset=40
          local.get 0
          local.get 0
          i32.const 16
          i32.add
          i32.store offset=56
          local.get 5
          local.get 4
          local.get 0
          i32.const 40
          i32.add
          call 8
          br_if 1 (;@2;)
        end
        local.get 1
        i32.load offset=12
        local.set 1
        local.get 0
        i32.const 36
        i32.add
        i32.const 27
        i32.store
        local.get 0
        i32.const 28
        i32.add
        i32.const 27
        i32.store
        local.get 0
        local.get 1
        i32.const 12
        i32.add
        i32.store offset=32
        local.get 0
        local.get 1
        i32.const 8
        i32.add
        i32.store offset=24
        local.get 0
        i32.const 44
        i32.store offset=20
        local.get 0
        local.get 1
        i32.store offset=16
        local.get 0
        i32.const 3
        i32.store offset=60
        local.get 0
        i32.const 3
        i32.store offset=52
        local.get 0
        i32.const 1050052
        i32.store offset=48
        local.get 0
        i32.const 0
        i32.store offset=40
        local.get 0
        local.get 0
        i32.const 16
        i32.add
        i32.store offset=56
        local.get 5
        local.get 4
        local.get 0
        i32.const 40
        i32.add
        call 8
        local.set 3
      end
      local.get 0
      i32.const -64
      i32.sub
      global.set 0
      block  ;; label = @2
        local.get 3
        i32.eqz
        if  ;; label = @3
          local.get 2
          i32.load offset=16
          local.get 2
          i32.load offset=24
          local.tee 0
          i32.sub
          i32.const 9
          i32.le_u
          if  ;; label = @4
            local.get 2
            i32.const 16
            i32.add
            local.get 0
            i32.const 10
            call 19
            local.get 2
            i32.load offset=24
            local.set 0
          end
          local.get 2
          i32.load offset=20
          local.get 0
          i32.add
          local.tee 1
          i32.const 1049356
          i64.load align=1
          i64.store align=1
          local.get 1
          i32.const 8
          i32.add
          i32.const 1049364
          i32.load16_u align=1
          i32.store16 align=1
          local.get 2
          local.get 0
          i32.const 10
          i32.add
          i32.store offset=24
          local.get 2
          i32.const 8
          i32.add
          call 0
          local.tee 4
          call 1
          local.get 2
          i32.load offset=8
          local.set 6
          local.get 2
          i32.load offset=12
          local.tee 5
          local.get 2
          i32.load offset=16
          local.get 2
          i32.load offset=24
          local.tee 0
          i32.sub
          i32.gt_u
          if  ;; label = @4
            local.get 2
            i32.const 16
            i32.add
            local.get 0
            local.get 5
            call 19
            local.get 2
            i32.load offset=24
            local.set 0
          end
          local.get 2
          i32.load offset=20
          local.get 0
          i32.add
          local.get 6
          local.get 5
          call 91
          drop
          local.get 2
          local.get 0
          local.get 5
          i32.add
          local.tee 0
          i32.store offset=24
          local.get 2
          i32.load offset=16
          local.get 0
          i32.sub
          i32.const 1
          i32.le_u
          if  ;; label = @4
            local.get 2
            i32.const 16
            i32.add
            local.get 0
            i32.const 2
            call 19
            local.get 2
            i32.load offset=24
            local.set 0
          end
          local.get 2
          i32.load offset=20
          local.get 0
          i32.add
          i32.const 2570
          i32.store16 align=1
          local.get 2
          local.get 0
          i32.const 2
          i32.add
          local.tee 3
          i32.store offset=24
          local.get 2
          i32.load offset=20
          local.set 0
          block  ;; label = @4
            local.get 3
            local.get 2
            i32.load offset=16
            local.tee 1
            i32.ge_u
            if  ;; label = @5
              local.get 0
              local.set 1
              br 1 (;@4;)
            end
            local.get 3
            i32.eqz
            if  ;; label = @5
              i32.const 1
              local.set 1
              local.get 0
              call 6
              br 1 (;@4;)
            end
            local.get 0
            local.get 1
            i32.const 1
            local.get 3
            call 67
            local.tee 1
            i32.eqz
            br_if 2 (;@2;)
          end
          local.get 1
          local.get 3
          call 2
          local.get 5
          if  ;; label = @4
            local.get 6
            call 6
          end
          local.get 4
          i32.const 132
          i32.ge_u
          if  ;; label = @4
            local.get 4
            call 3
          end
          local.get 2
          i32.const 80
          i32.add
          global.set 0
          br 2 (;@1;)
        end
        global.get 0
        i32.const -64
        i32.add
        local.tee 0
        global.set 0
        local.get 0
        i32.const 55
        i32.store offset=12
        local.get 0
        i32.const 1049192
        i32.store offset=8
        local.get 0
        i32.const 1049248
        i32.store offset=20
        local.get 0
        local.get 2
        i32.const 72
        i32.add
        i32.store offset=16
        local.get 0
        i32.const 36
        i32.add
        i32.const 2
        i32.store
        local.get 0
        i32.const 44
        i32.add
        i32.const 2
        i32.store
        local.get 0
        i32.const 60
        i32.add
        i32.const 43
        i32.store
        local.get 0
        i32.const 1050128
        i32.store offset=32
        local.get 0
        i32.const 0
        i32.store offset=24
        local.get 0
        i32.const 44
        i32.store offset=52
        local.get 0
        local.get 0
        i32.const 48
        i32.add
        i32.store offset=40
        local.get 0
        local.get 0
        i32.const 16
        i32.add
        i32.store offset=56
        local.get 0
        local.get 0
        i32.const 8
        i32.add
        i32.store offset=48
        local.get 0
        i32.const 24
        i32.add
        i32.const 1049340
        call 43
        unreachable
      end
      local.get 3
      i32.const 1
      call 90
      unreachable
    end)
  (func (;99;) (type 7) (param i32) (result i64)
    i64.const -1568163068372127146)
  (func (;100;) (type 7) (param i32) (result i64)
    i64.const -8661331358421426486)
  (func (;101;) (type 7) (param i32) (result i64)
    i64.const -8527728395957036344)
  (func (;102;) (type 4) (param i32)
    nop)
  (table (;0;) 49 49 funcref)
  (memory (;0;) 17)
  (global (;0;) (mut i32) (i32.const 1048576))
  (export "memory" (memory 0))
  (export "__wbg_universe_free" (func 38))
  (export "universe_tick" (func 45))
  (export "universe_new" (func 18))
  (export "universe_width" (func 49))
  (export "universe_set_width" (func 23))
  (export "universe_height" (func 46))
  (export "universe_set_height" (func 24))
  (export "universe_cells" (func 47))
  (export "universe_toggle_cell" (func 29))
  (export "__wbindgen_free" (func 68))
  (export "__wbindgen_malloc" (func 44))
  (export "__wbindgen_realloc" (func 51))
  (elem (;0;) (i32.const 1) func 102 98 98 98 102 36 11 32 60 39 13 34 102 71 55 27 54 55 53 62 61 54 54 57 56 58 82 30 102 37 12 33 60 100 101 50 17 26 41 74 99 81 72 76 31 70 102 99)
  (data (;0;) (i32.const 1048576) "src/lib.rs\00\00\00\00\10\00\0a\00\00\00c\00\00\00\12\00\00\00\00\00\10\00\0a\00\00\00f\00\00\00\12\00\00\00\00\00\10\00\0a\00\00\00i\00\00\00\12\00\00\00\00\00\10\00\0a\00\00\00l\00\00\00\12\00\00\00\00\00\10\00\0a\00\00\00o\00\00\00\12\00\00\00\00\00\10\00\0a\00\00\00r\00\00\00\12\00\00\00\00\00\10\00\0a\00\00\00u\00\00\00\12\00\00\00\00\00\10\00\0a\00\00\00x\00\00\00\12\00\00\00\00\00\10\00\0a\00\00\00\89\00\00\00\1c\00\00\00/Users/vigoo/.cargo/registry/src/github.com-1ecc6299db9ec823/console_error_panic_hook-0.1.7/src/lib.rs\00\00\9c\00\10\00f\00\00\00\95\00\00\00\0e\00\00\00\00\00\10\00\0a\00\00\00\d9\00\00\00\09\00\00\00\01\00\00\00\00\00\00\00\01\00\00\00\02\00\00\00\03\00\00\00\04\00\00\00one-time initialization may not be performed recursively<\01\10\008\00\00\00Once instance has previously been poisoned\00\00|\01\10\00*\00\00\00called `Option::unwrap()` on a `None` value/rustc/84c898d65adf2f39a5a98507f1fe0ce10a2b8dbc/library/std/src/sync/once.rs\00\db\01\10\00L\00\00\00\95\00\00\002\00\00\00\05\00\00\00\04\00\00\00\04\00\00\00\06\00\00\00\07\00\00\00\08\00\00\00\09\00\00\00\0c\00\00\00\04\00\00\00\0a\00\00\00\0b\00\00\00\0c\00\00\00a Display implementation returned an error unexpectedly\00\0d\00\00\00\00\00\00\00\01\00\00\00\0e\00\00\00/rustc/84c898d65adf2f39a5a98507f1fe0ce10a2b8dbc/library/alloc/src/string.rs\00\b0\02\10\00K\00\00\00\e5\09\00\00\0e\00\00\00\0a\0aStack:\0a\0aclosure invoked recursively or after being droppednull pointer passed to rustrecursive use of an object detected which would lead to unsafe aliasing in rust\00\00\1d\00\00\00\04\00\00\00\04\00\00\00\1e\00\00\00\1f\00\00\00 \00\00\00called `Option::unwrap()` on a `None` valuememory allocation of  bytes failed\00\00\00\f7\03\10\00\15\00\00\00\0c\04\10\00\0d\00\00\00library/std/src/alloc.rs,\04\10\00\18\00\00\00U\01\00\00\09\00\00\00cannot modify the panic hook from a panicking threadT\04\10\004\00\00\00library/std/src/panicking.rs\90\04\10\00\1c\00\00\00\89\00\00\00\09\00\00\00\90\04\10\00\1c\00\00\00B\02\00\00\1e\00\00\00\90\04\10\00\1c\00\00\00A\02\00\00\1f\00\00\00!\00\00\00\0c\00\00\00\04\00\00\00\22\00\00\00\1d\00\00\00\08\00\00\00\04\00\00\00#\00\00\00$\00\00\00\10\00\00\00\04\00\00\00%\00\00\00&\00\00\00\1d\00\00\00\08\00\00\00\04\00\00\00'\00\00\00(\00\00\00\1d\00\00\00\00\00\00\00\01\00\00\00)\00\00\00library/alloc/src/raw_vec.rscapacity overflow\00\00\00P\05\10\00\11\00\00\004\05\10\00\1c\00\00\00\0d\02\00\00\05\00\00\00index out of bounds: the len is  but the index is \00\00|\05\10\00 \00\00\00\9c\05\10\00\12\00\00\00:\00\00\00|\05\10\00\00\00\00\00\c0\05\10\00\01\00\00\00\c0\05\10\00\01\00\00\00panicked at '', \e8\05\10\00\01\00\00\00\e9\05\10\00\03\00\00\00/\00\00\00\00\00\00\00\01\00\00\000\00\00\00: \00\00|\05\10\00\00\00\00\00\0c\06\10\00\02\00\00\0000010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899Error"))
