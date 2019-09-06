(**
   Copyright 2019 Google LLC

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

module Option = struct
  let ( >>= ) m f = match m with Some a -> f a | None -> None

  let ( >>? ) m v = match m with Some x -> Some x | None -> v
end

module Result = struct
  let ( >>= ) m f = match m with Ok a -> f a | Error e -> Error e

  let from_opt o err = match o with Some a -> Ok a | None -> err
end
