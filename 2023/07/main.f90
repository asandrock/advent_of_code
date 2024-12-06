program main
  use StringiFor
  use PENF
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: lines(:), parts(:), hand(:)
  integer :: j, k, n_hands, tmp
  integer, allocatable :: rank(:), bid(:), sorted_indices(:)
  logical :: swapped
  character, dimension(*), parameter :: cards = &
    & ['A','K','Q','J','T','9','8','7','6','5','4','3','2']

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))

  n_hands = size(lines)
  allocate(bid(n_hands), hand(n_hands))
  do j = 1, n_hands
    call lines(j)%split(tokens=parts, sep=' ')
    if (size(parts) /= 2) stop 'More than two parts'
    hand(j) = parts(1)
    bid(j) = parts(2)%to_number(kind=I4P)    
  end do

  ! Sort hands (Bubble sort algorithm)
  allocate(sorted_indices(n_hands), rank(n_hands))
  do j = 1, n_hands
    sorted_indices(j) = j
  end do
  outer: do j = 1, n_hands
    swapped = .false.
    inner: do k = n_hands, j + 1, -1
      if (higher(hand(sorted_indices(k - 1)), hand(sorted_indices(k)))) then
        tmp = sorted_indices(k - 1)
        sorted_indices(k - 1) = sorted_indices(k)
        sorted_indices(k) = tmp
        swapped = .true.
      end if
    end do inner
    if (.not. swapped) exit outer
  end do outer

  do j = 1, n_hands
    rank(j) = sorted_indices(n_hands - j + 1)
  end do
  do j = 1, n_hands
    print *, j, hand(sorted_indices(n_hands - j + 1)), &
      bid(sorted_indices(n_hands - j + 1))
  end do
  print *, '======='
  print *, sum(rank*bid)
contains
  function count_cards(hand)
    type(string), intent(in) :: hand
    integer, dimension(size(cards)) :: count_cards

    integer :: j, k

    count_cards = 0
    do j = 1, hand%len()
      do k = 1, size(cards)
        if (hand%raw(j:j) == cards(k)) then
          count_cards(k) = count_cards(k) + 1
        end if
      end do
    end do
  end function count_cards

  function higher(hand1, hand2)
    ! Test whether hand2 is higher than hand1
    logical :: higher
    type(string), intent(in) :: hand1, hand2

    integer, dimension(size(cards), 2) :: counts
    integer :: j, types(2)

    integer, parameter :: high_card = 0, one_pair = 1, two_pairs = 2, &
      three_of_a_kind = 3, full_house = 4, four_of_a_kind = 5, &
      five_of_a_kind = 6

    counts(:, 1) = count_cards(hand1)
    counts(:, 2) = count_cards(hand2)

    do j = 1, 2
      if (maxval(counts(:, j)) == 1) then
        types(j) = high_card
      else if (maxval(counts(:, j)) == 2) then
        if (count(counts(:,j) == 2) == 1) then
          types(j) = one_pair
        else
          types(j) = two_pairs
        end if
      else if (maxval(counts(:, j)) == 3) then
        if (maxval(counts(:, j), mask=(counts(:,j) < maxval(counts(:, j)))) == 2) then
          types(j) = full_house
        else
          types(j) = three_of_a_kind
        end if
      else if (maxval(counts(:, j)) == 4) then
        types(j) = four_of_a_kind
      else
        types(j) = five_of_a_kind
      end if
    end do

    if (types(1) /= types(2)) then
      higher = types(2) > types(1)
    else
      card_loop: do j = 1, hand1%len()
        if (hand1%raw(j:j) /= hand2%raw(j:j)) then
          higher = higher_card(hand1%raw(j:j), hand2%raw(j:j))
          exit card_loop
        end if
      end do card_loop
    end if
  end function higher

  function higher_card(card1, card2)
    logical :: higher_card
    character, intent(in) :: card1, card2

    integer, dimension(size(cards)) :: count1, count2

    count1 = count_cards(string(card1))
    count2 = count_cards(string(card2))
    higher_card = all(maxloc(count2) < maxloc(count1))
  end function higher_card
end program main
